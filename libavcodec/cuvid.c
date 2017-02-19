/*
 * Nvidia CUVID decoder
 * Copyright (c) 2016 Timo Rothenpieler <timo@rothenpieler.org>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <stdint.h>
#include <string.h>

#include "compat/cuda/dynlink_loader.h"

#include "libavutil/buffer.h"
#include "libavutil/mathematics.h"
#include "libavutil/hwcontext.h"
#include "libavutil/hwcontext_cuda_internal.h"
#include "libavutil/fifo.h"
#include "libavutil/log.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"

#include "h264dec.h"
#include "mpegvideo.h"
#include "avcodec.h"
#include "internal.h"

#define MAX_FRAME_COUNT 25
#define A53_QUEUE_SIZE (MAX_FRAME_COUNT + 8)

typedef struct CuvidContext
{
    AVClass *avclass;

    CUvideodecoder cudecoder;
    CUvideoparser cuparser;

    char *cu_gpu;
    int nb_surfaces;
    int drop_second_field;

    AVBufferRef *hwdevice;
    AVBufferRef *hwframe;

    AVBSFContext *bsf;

    AVFifoBuffer *frame_queue;

    int deint_mode;
    int deint_mode_current;
    int64_t prev_pts;

    int internal_error;
    int decoder_flushing;

    cudaVideoCodec codec_type;
    cudaVideoChromaFormat chroma_format;

    uint8_t* a53_caption;
    int a53_caption_size;
    uint8_t* a53_caption_queue[A53_QUEUE_SIZE];
    int a53_caption_size_queue[A53_QUEUE_SIZE];

    CUVIDPARSERPARAMS cuparseinfo;
    CUVIDEOFORMATEX cuparse_ext;

    CudaFunctions *cudl;
    CuvidFunctions *cvdl;
} CuvidContext;

typedef struct CuvidParsedFrame
{
    CUVIDPARSERDISPINFO dispinfo;
    int second_field;
    int is_deinterlacing;
    uint8_t* a53_caption;
    int a53_caption_size;
} CuvidParsedFrame;

typedef struct CUVIDFrame {
    int idx;
    AVFrame *f;
} CUVIDFrame;

typedef struct CUVIDContext {
    CudaFunctions *cudl;
    CuvidFunctions *cvdl;
    CUcontext      cuda_ctx;
    CUvideodecoder decoder;
    CUVIDPICPARAMS pic_params;

    uint8_t     *bitstream;
    int          bitstream_len;
    unsigned int bitstream_allocated;

    unsigned    *slice_offsets;
    int          nb_slices;
    unsigned int slice_offsets_allocated;
} CUVIDContext;

static int check_cu(AVCodecContext *avctx, CUresult err, const char *func)
{
    CuvidContext *ctx = avctx->priv_data;
    const char *err_name;
    const char *err_string;

    av_log(avctx, AV_LOG_TRACE, "Calling %s\n", func);

    if (err == CUDA_SUCCESS)
        return 0;

    ctx->cudl->cuGetErrorName(err, &err_name);
    ctx->cudl->cuGetErrorString(err, &err_string);

    av_log(avctx, AV_LOG_ERROR, "%s failed", func);
    if (err_name && err_string)
        av_log(avctx, AV_LOG_ERROR, " -> %s: %s", err_name, err_string);
    av_log(avctx, AV_LOG_ERROR, "\n");

    return AVERROR_EXTERNAL;
}

#define CHECK_CU(x) check_cu(avctx, (x), #x)

static int CUDAAPI cuvid_handle_video_sequence(void *opaque, CUVIDEOFORMAT* format)
{
    AVCodecContext *avctx = opaque;
    CuvidContext *ctx = avctx->priv_data;
    AVHWFramesContext *hwframe_ctx = (AVHWFramesContext*)ctx->hwframe->data;
    CUVIDDECODECREATEINFO cuinfo;
    int surface_fmt;

    enum AVPixelFormat pix_fmts[3] = { AV_PIX_FMT_CUDA,
                                       AV_PIX_FMT_NONE,  // Will be updated below
                                       AV_PIX_FMT_NONE };

    av_log(avctx, AV_LOG_TRACE, "pfnSequenceCallback, progressive_sequence=%d\n", format->progressive_sequence);

    ctx->internal_error = 0;

    switch (format->bit_depth_luma_minus8) {
    case 0: // 8-bit
        pix_fmts[1] = AV_PIX_FMT_NV12;
        break;
    case 2: // 10-bit
        pix_fmts[1] = AV_PIX_FMT_P010;
        break;
    case 4: // 12-bit
        pix_fmts[1] = AV_PIX_FMT_P016;
        break;
    default:
        av_log(avctx, AV_LOG_ERROR, "unsupported bit depth: %d\n",
               format->bit_depth_luma_minus8 + 8);
        ctx->internal_error = AVERROR(EINVAL);
        return 0;
    }
    surface_fmt = ff_get_format(avctx, pix_fmts);
    if (surface_fmt < 0) {
        av_log(avctx, AV_LOG_ERROR, "ff_get_format failed: %d\n", surface_fmt);
        ctx->internal_error = AVERROR(EINVAL);
        return 0;
    }

    av_log(avctx, AV_LOG_VERBOSE, "Formats: Original: %s | HW: %s | SW: %s\n",
           av_get_pix_fmt_name(avctx->pix_fmt),
           av_get_pix_fmt_name(surface_fmt),
           av_get_pix_fmt_name(avctx->sw_pix_fmt));

    avctx->pix_fmt = surface_fmt;

    avctx->width = format->display_area.right;
    avctx->height = format->display_area.bottom;

    ff_set_sar(avctx, av_div_q(
        (AVRational){ format->display_aspect_ratio.x, format->display_aspect_ratio.y },
        (AVRational){ avctx->width, avctx->height }));

    ctx->deint_mode_current = format->progressive_sequence
                              ? cudaVideoDeinterlaceMode_Weave
                              : ctx->deint_mode;

    if (!format->progressive_sequence && ctx->deint_mode_current == cudaVideoDeinterlaceMode_Weave)
        avctx->flags |= AV_CODEC_FLAG_INTERLACED_DCT;
    else
        avctx->flags &= ~AV_CODEC_FLAG_INTERLACED_DCT;

    if (format->video_signal_description.video_full_range_flag)
        avctx->color_range = AVCOL_RANGE_JPEG;
    else
        avctx->color_range = AVCOL_RANGE_MPEG;

    avctx->color_primaries = format->video_signal_description.color_primaries;
    avctx->color_trc = format->video_signal_description.transfer_characteristics;
    avctx->colorspace = format->video_signal_description.matrix_coefficients;

    if (format->bitrate)
        avctx->bit_rate = format->bitrate;

    if (format->frame_rate.numerator && format->frame_rate.denominator) {
        avctx->framerate.num = format->frame_rate.numerator;
        avctx->framerate.den = format->frame_rate.denominator;
    }

    if (ctx->cudecoder
            && avctx->coded_width == format->coded_width
            && avctx->coded_height == format->coded_height
            && ctx->chroma_format == format->chroma_format
            && ctx->codec_type == format->codec)
        return 1;

    if (ctx->cudecoder) {
        av_log(avctx, AV_LOG_TRACE, "Re-initializing decoder\n");
        ctx->internal_error = CHECK_CU(ctx->cvdl->cuvidDestroyDecoder(ctx->cudecoder));
        if (ctx->internal_error < 0)
            return 0;
        ctx->cudecoder = NULL;
    }

    if (hwframe_ctx->pool && (
            hwframe_ctx->width < avctx->width ||
            hwframe_ctx->height < avctx->height ||
            hwframe_ctx->format != AV_PIX_FMT_CUDA ||
            hwframe_ctx->sw_format != avctx->sw_pix_fmt)) {
        av_log(avctx, AV_LOG_ERROR, "AVHWFramesContext is already initialized with incompatible parameters\n");
        ctx->internal_error = AVERROR(EINVAL);
        return 0;
    }

    if (format->chroma_format != cudaVideoChromaFormat_420) {
        av_log(avctx, AV_LOG_ERROR, "Chroma formats other than 420 are not supported\n");
        ctx->internal_error = AVERROR(EINVAL);
        return 0;
    }

    avctx->coded_width = format->coded_width;
    avctx->coded_height = format->coded_height;

    ctx->chroma_format = format->chroma_format;

    memset(&cuinfo, 0, sizeof(cuinfo));

    cuinfo.CodecType = ctx->codec_type = format->codec;
    cuinfo.ChromaFormat = format->chroma_format;

    switch (avctx->sw_pix_fmt) {
    case AV_PIX_FMT_NV12:
        cuinfo.OutputFormat = cudaVideoSurfaceFormat_NV12;
        break;
    case AV_PIX_FMT_P010:
    case AV_PIX_FMT_P016:
        cuinfo.OutputFormat = cudaVideoSurfaceFormat_P016;
        break;
    default:
        av_log(avctx, AV_LOG_ERROR, "Output formats other than NV12, P010 or P016 are not supported\n");
        ctx->internal_error = AVERROR(EINVAL);
        return 0;
    }

    cuinfo.ulWidth = avctx->coded_width;
    cuinfo.ulHeight = avctx->coded_height;
    cuinfo.ulTargetWidth = cuinfo.ulWidth;
    cuinfo.ulTargetHeight = cuinfo.ulHeight;

    cuinfo.target_rect.left = 0;
    cuinfo.target_rect.top = 0;
    cuinfo.target_rect.right = cuinfo.ulWidth;
    cuinfo.target_rect.bottom = cuinfo.ulHeight;

    cuinfo.ulNumDecodeSurfaces = ctx->nb_surfaces;
    cuinfo.ulNumOutputSurfaces = 1;
    cuinfo.ulCreationFlags = cudaVideoCreate_PreferCUVID;
    cuinfo.bitDepthMinus8 = format->bit_depth_luma_minus8;
    cuinfo.DeinterlaceMode = ctx->deint_mode_current;

    if (ctx->deint_mode_current != cudaVideoDeinterlaceMode_Weave && !ctx->drop_second_field)
        avctx->framerate = av_mul_q(avctx->framerate, (AVRational){2, 1});

    ctx->internal_error = CHECK_CU(ctx->cvdl->cuvidCreateDecoder(&ctx->cudecoder, &cuinfo));
    if (ctx->internal_error < 0)
        return 0;

    if (!hwframe_ctx->pool) {
        hwframe_ctx->format = AV_PIX_FMT_CUDA;
        hwframe_ctx->sw_format = avctx->sw_pix_fmt;
        hwframe_ctx->width = avctx->width;
        hwframe_ctx->height = avctx->height;

        if ((ctx->internal_error = av_hwframe_ctx_init(ctx->hwframe)) < 0) {
            av_log(avctx, AV_LOG_ERROR, "av_hwframe_ctx_init failed\n");
            return 0;
        }
    }

    return 1;
}

static int CUDAAPI cuvid_handle_picture_decode(void *opaque, CUVIDPICPARAMS* picparams)
{
    AVCodecContext *avctx = opaque;
    CuvidContext *ctx = avctx->priv_data;

    av_log(avctx, AV_LOG_TRACE, "pfnDecodePicture\n");

    if (ctx->a53_caption)
    {

        if (picparams->CurrPicIdx >= A53_QUEUE_SIZE)
        {
            av_log(avctx, AV_LOG_WARNING, "CurrPicIdx too big: %d\n", picparams->CurrPicIdx);
            av_freep(&ctx->a53_caption);
        }
        else
        {
            int pos = picparams->CurrPicIdx;
            av_freep(&ctx->a53_caption_queue[pos]);
            ctx->a53_caption_queue[pos] = ctx->a53_caption;
            ctx->a53_caption_size_queue[pos] = ctx->a53_caption_size;
            ctx->a53_caption = NULL;
        }
    }

    ctx->internal_error = CHECK_CU(ctx->cvdl->cuvidDecodePicture(ctx->cudecoder, picparams));
    if (ctx->internal_error < 0)
        return 0;

    return 1;
}

static int CUDAAPI cuvid_handle_picture_display(void *opaque, CUVIDPARSERDISPINFO* dispinfo)
{
    AVCodecContext *avctx = opaque;
    CuvidContext *ctx = avctx->priv_data;
    CuvidParsedFrame parsed_frame = { { 0 } };
    uint8_t* a53_caption = NULL;
    int a53_caption_size = 0;

    if (dispinfo->picture_index >= A53_QUEUE_SIZE)
    {
        av_log(avctx, AV_LOG_WARNING, "picture_index too big: %d\n", dispinfo->picture_index);
    }
    else
    {
        int pos = dispinfo->picture_index;
        a53_caption = ctx->a53_caption_queue[pos];
        a53_caption_size = ctx->a53_caption_size_queue[pos];
        ctx->a53_caption_queue[pos] = NULL;
    }

    parsed_frame.dispinfo = *dispinfo;
    ctx->internal_error = 0;

    if (ctx->deint_mode_current == cudaVideoDeinterlaceMode_Weave) {
        parsed_frame.a53_caption = a53_caption;
        parsed_frame.a53_caption_size = a53_caption_size;
        av_fifo_generic_write(ctx->frame_queue, &parsed_frame, sizeof(CuvidParsedFrame), NULL);
    } else {
        parsed_frame.is_deinterlacing = 1;
        parsed_frame.a53_caption = a53_caption;
        parsed_frame.a53_caption_size = a53_caption_size;
        av_fifo_generic_write(ctx->frame_queue, &parsed_frame, sizeof(CuvidParsedFrame), NULL);
	if (!ctx->drop_second_field) {
		parsed_frame.a53_caption = NULL;
		parsed_frame.a53_caption_size = 0;
		parsed_frame.second_field = 1;
		av_fifo_generic_write(ctx->frame_queue, &parsed_frame, sizeof(CuvidParsedFrame), NULL);
	}
    }

    return 1;
}

static void cuvid_mpeg_parse_a53(CuvidContext *ctx, const uint8_t* p, int buf_size)
{
    const uint8_t* buf_end = p + buf_size;
    for(;;)
    {
        uint32_t start_code = -1;
        p = avpriv_find_start_code(p, buf_end, &start_code);
        if (start_code > 0x1ff)
            break;
        if (start_code != 0x1b2)
            continue;
        buf_size = buf_end - p;
        if (buf_size >= 6 &&
            p[0] == 'G' && p[1] == 'A' && p[2] == '9' && p[3] == '4' && p[4] == 3 && (p[5] & 0x40))
        {
            /* extract A53 Part 4 CC data */
            int cc_count = p[5] & 0x1f;
            if (cc_count > 0 && buf_size >= 7 + cc_count * 3)
            {
                av_freep(&ctx->a53_caption);
                ctx->a53_caption_size = cc_count * 3;
                ctx->a53_caption      = av_malloc(ctx->a53_caption_size);
                if (ctx->a53_caption)
                    memcpy(ctx->a53_caption, p + 7, ctx->a53_caption_size);
            }
        }
        else if (buf_size >= 11 && p[0] == 'C' && p[1] == 'C' && p[2] == 0x01 && p[3] == 0xf8)
        {
            int cc_count = 0;
            int i;
            // There is a caption count field in the data, but it is often
            // incorrect.  So count the number of captions present.
            for (i = 5; i + 6 <= buf_size && ((p[i] & 0xfe) == 0xfe); i += 6)
                cc_count++;
            // Transform the DVD format into A53 Part 4 format
            if (cc_count > 0) {
                av_freep(&ctx->a53_caption);
                ctx->a53_caption_size = cc_count * 6;
                ctx->a53_caption      = av_malloc(ctx->a53_caption_size);
                if (ctx->a53_caption) {
                    uint8_t field1 = !!(p[4] & 0x80);
                    uint8_t *cap = ctx->a53_caption;
                    p += 5;
                    for (i = 0; i < cc_count; i++)
                    {
                        cap[0] = (p[0] == 0xff && field1) ? 0xfc : 0xfd;
                        cap[1] = p[1];
                        cap[2] = p[2];
                        cap[3] = (p[3] == 0xff && !field1) ? 0xfc : 0xfd;
                        cap[4] = p[4];
                        cap[5] = p[5];
                        cap += 6;
                        p += 6;
                    }
                }
            }
        }
    }
}


static void cuvid_h264_parse_a53(CuvidContext *ctx, const uint8_t* p, int buf_size)
{
    const uint8_t* buf_end = p + buf_size;
    while(p < buf_end)
    {
        int i, size, cc_count;
        uint32_t start_code = -1;
	uint64_t new_size;
        p = avpriv_find_start_code(p, buf_end, &start_code);
            if (start_code > 0x1ff)
            break;
        if (start_code != 0x106)
            continue;
        buf_size = buf_end - p;
        if (buf_size < 1 || p[0] != 4)
            continue;
        p += 1; buf_size -= 1;
        size = 0;
        while (buf_size > 0)
        {
            size += p[0];
            buf_size -= 1;
            if (*(p++) != 0xFF)
                break;
        }
        if (buf_size <= 0 || buf_size < size)
            continue;
        if (size < 7)
            continue;
        if (p[0] == 0xFF)
        {
            p+=4;
            size-=4;
        }
        else
        {
            p+=3;
            size-=3;
        }
        if (p[0] != 'G' || p[1] != 'A' || p[2] != '9' || p[3] != '4')
            continue;
        p += 4;
        size -= 4;

        if (size < 3)
            continue;
        if (p[0] != 3)
            continue;
        if (!(p[1] & 0x40))
            continue;
        cc_count = p[1] & 0x1F;
        p+=3;
        size -= 3;

        if (!cc_count || size < cc_count * 3)
            continue;

        if (!ctx->a53_caption)
            ctx->a53_caption_size = 0;
        new_size = (ctx->a53_caption_size + cc_count * 3);
        if (av_reallocp(&ctx->a53_caption, new_size) < 0)
            continue;
        for(i = 0; i < cc_count; ++i, p += 3)
        {
            ctx->a53_caption[ctx->a53_caption_size++] = p[0];
            ctx->a53_caption[ctx->a53_caption_size++] = p[1];
            ctx->a53_caption[ctx->a53_caption_size++] = p[2];
        }
    }
}


static int cuvid_decode_packet(AVCodecContext *avctx, const AVPacket *avpkt)
{
    CuvidContext *ctx = avctx->priv_data;
    AVHWDeviceContext *device_ctx = (AVHWDeviceContext*)ctx->hwdevice->data;
    AVCUDADeviceContext *device_hwctx = device_ctx->hwctx;
    CUcontext dummy, cuda_ctx = device_hwctx->cuda_ctx;
    CUVIDSOURCEDATAPACKET cupkt;
    AVPacket filter_packet = { 0 };
    AVPacket filtered_packet = { 0 };
    int ret = 0, eret = 0, is_flush = ctx->decoder_flushing;

    av_log(avctx, AV_LOG_TRACE, "cuvid_decode_packet\n");

    if (is_flush && avpkt && avpkt->size)
        return AVERROR_EOF;

    if ((av_fifo_size(ctx->frame_queue) / sizeof(CuvidParsedFrame)) + 2 > ctx->nb_surfaces && avpkt && avpkt->size)
        return AVERROR(EAGAIN);

    if (ctx->bsf && avpkt && avpkt->size) {
        if ((ret = av_packet_ref(&filter_packet, avpkt)) < 0) {
            av_log(avctx, AV_LOG_ERROR, "av_packet_ref failed\n");
            return ret;
        }

        if ((ret = av_bsf_send_packet(ctx->bsf, &filter_packet)) < 0) {
            av_log(avctx, AV_LOG_ERROR, "av_bsf_send_packet failed\n");
            av_packet_unref(&filter_packet);
            return ret;
        }

        if ((ret = av_bsf_receive_packet(ctx->bsf, &filtered_packet)) < 0) {
            av_log(avctx, AV_LOG_ERROR, "av_bsf_receive_packet failed\n");
            return ret;
        }

        avpkt = &filtered_packet;
    }

    ret = CHECK_CU(ctx->cudl->cuCtxPushCurrent(cuda_ctx));
    if (ret < 0) {
        av_packet_unref(&filtered_packet);
        return ret;
    }

    memset(&cupkt, 0, sizeof(cupkt));

    if (avpkt && avpkt->size) {
        cupkt.payload_size = avpkt->size;
        cupkt.payload = avpkt->data;

        if (avpkt->pts != AV_NOPTS_VALUE)
        {
            cupkt.flags = CUVID_PKT_TIMESTAMP;
            if (avctx->pkt_timebase.num && avctx->pkt_timebase.den)
                cupkt.timestamp = av_rescale_q(avpkt->pts, avctx->pkt_timebase, (AVRational){1, 10000000});
            else
                cupkt.timestamp = avpkt->pts;
       }
    } else {
        cupkt.flags = CUVID_PKT_ENDOFSTREAM;
        ctx->decoder_flushing = 1;
    }

    ret = CHECK_CU(ctx->cvdl->cuvidParseVideoData(ctx->cuparser, &cupkt));

    // assume there is one frame delay (the parser outputs previous picture once it sees new frame data)
    av_freep(&ctx->a53_caption);
    if (avpkt && avpkt->size) {
        if (ctx->cuparseinfo.CodecType == cudaVideoCodec_MPEG2)
            cuvid_mpeg_parse_a53(ctx, avpkt->data, avpkt->size);
        else if (ctx->cuparseinfo.CodecType == cudaVideoCodec_H264)
            cuvid_h264_parse_a53(ctx, avpkt->data, avpkt->size);
    }

    av_packet_unref(&filtered_packet);

    if (ret < 0)
        goto error;

    // cuvidParseVideoData doesn't return an error just because stuff failed...
    if (ctx->internal_error) {
        av_log(avctx, AV_LOG_ERROR, "cuvid decode callback error\n");
        ret = ctx->internal_error;
        goto error;
    }

error:
    eret = CHECK_CU(ctx->cudl->cuCtxPopCurrent(&dummy));

    if (eret < 0)
        return eret;
    else if (ret < 0)
        return ret;
    else if (is_flush)
        return AVERROR_EOF;
    else
        return 0;
}

static int cuvid_output_frame(AVCodecContext *avctx, AVFrame *frame)
{
    CuvidContext *ctx = avctx->priv_data;
    AVHWDeviceContext *device_ctx = (AVHWDeviceContext*)ctx->hwdevice->data;
    AVCUDADeviceContext *device_hwctx = device_ctx->hwctx;
    CUcontext dummy, cuda_ctx = device_hwctx->cuda_ctx;
    CUdeviceptr mapped_frame = 0;
    int ret = 0, eret = 0;

    av_log(avctx, AV_LOG_TRACE, "cuvid_output_frame\n");

    if (ctx->decoder_flushing) {
        ret = cuvid_decode_packet(avctx, NULL);
        if (ret < 0 && ret != AVERROR_EOF)
            return ret;
    }

    ret = CHECK_CU(ctx->cudl->cuCtxPushCurrent(cuda_ctx));
    if (ret < 0)
        return ret;

    if (av_fifo_size(ctx->frame_queue)) {
        CuvidParsedFrame parsed_frame;
        CUVIDPROCPARAMS params;
        unsigned int pitch = 0;
        int offset = 0;
        int i;

        av_fifo_generic_read(ctx->frame_queue, &parsed_frame, sizeof(CuvidParsedFrame), NULL);

        memset(&params, 0, sizeof(params));
        params.progressive_frame = parsed_frame.dispinfo.progressive_frame;
        params.second_field = parsed_frame.second_field;
        params.top_field_first = parsed_frame.dispinfo.top_field_first;

        ret = CHECK_CU(ctx->cvdl->cuvidMapVideoFrame(ctx->cudecoder, parsed_frame.dispinfo.picture_index, &mapped_frame, &pitch, &params));
        if (ret < 0)
            goto error;

        if (avctx->pix_fmt == AV_PIX_FMT_CUDA) {
            ret = av_hwframe_get_buffer(ctx->hwframe, frame, 0);
            if (ret < 0) {
                av_log(avctx, AV_LOG_ERROR, "av_hwframe_get_buffer failed\n");
                goto error;
            }

            ret = ff_decode_frame_props(avctx, frame);
            if (ret < 0) {
                av_log(avctx, AV_LOG_ERROR, "ff_decode_frame_props failed\n");
                goto error;
            }

            for (i = 0; i < 2; i++) {
                CUDA_MEMCPY2D cpy = {
                    .srcMemoryType = CU_MEMORYTYPE_DEVICE,
                    .dstMemoryType = CU_MEMORYTYPE_DEVICE,
                    .srcDevice     = mapped_frame,
                    .dstDevice     = (CUdeviceptr)frame->data[i],
                    .srcPitch      = pitch,
                    .dstPitch      = frame->linesize[i],
                    .srcY          = offset,
                    .WidthInBytes  = FFMIN(pitch, frame->linesize[i]),
                    .Height        = avctx->height >> (i ? 1 : 0),
                };

                ret = CHECK_CU(ctx->cudl->cuMemcpy2D(&cpy));
                if (ret < 0)
                    goto error;

                offset += avctx->coded_height;
            }
        } else if (avctx->pix_fmt == AV_PIX_FMT_NV12 ||
                   avctx->pix_fmt == AV_PIX_FMT_P010 ||
                   avctx->pix_fmt == AV_PIX_FMT_P016) {
            AVFrame *tmp_frame = av_frame_alloc();
            if (!tmp_frame) {
                av_log(avctx, AV_LOG_ERROR, "av_frame_alloc failed\n");
                ret = AVERROR(ENOMEM);
                goto error;
            }

            tmp_frame->format        = AV_PIX_FMT_CUDA;
            tmp_frame->hw_frames_ctx = av_buffer_ref(ctx->hwframe);
            tmp_frame->data[0]       = (uint8_t*)mapped_frame;
            tmp_frame->linesize[0]   = pitch;
            tmp_frame->data[1]       = (uint8_t*)(mapped_frame + avctx->coded_height * pitch);
            tmp_frame->linesize[1]   = pitch;
            tmp_frame->width         = avctx->width;
            tmp_frame->height        = avctx->height;

            ret = ff_get_buffer(avctx, frame, 0);
            if (ret < 0) {
                av_log(avctx, AV_LOG_ERROR, "ff_get_buffer failed\n");
                av_frame_free(&tmp_frame);
                goto error;
            }

            ret = av_hwframe_transfer_data(frame, tmp_frame, 0);
            if (ret) {
                av_log(avctx, AV_LOG_ERROR, "av_hwframe_transfer_data failed\n");
                av_frame_free(&tmp_frame);
                goto error;
            }
            av_frame_free(&tmp_frame);
        } else {
            ret = AVERROR_BUG;
            goto error;
        }

        frame->width = avctx->width;
        frame->height = avctx->height;
        if (avctx->pkt_timebase.num && avctx->pkt_timebase.den)
            frame->pts = av_rescale_q(parsed_frame.dispinfo.timestamp, (AVRational){1, 10000000}, avctx->pkt_timebase);
        else
            frame->pts = parsed_frame.dispinfo.timestamp;

        if (parsed_frame.second_field) {
            if (ctx->prev_pts == INT64_MIN) {
                ctx->prev_pts = frame->pts;
                frame->pts += (avctx->pkt_timebase.den * avctx->framerate.den) / (avctx->pkt_timebase.num * avctx->framerate.num);
            } else {
                int pts_diff = (frame->pts - ctx->prev_pts) / 2;
                ctx->prev_pts = frame->pts;
                frame->pts += pts_diff;
            }
        }

        /* CUVIDs opaque reordering breaks the internal pkt logic.
         * So set pkt_pts and clear all the other pkt_ fields.
         */
#if FF_API_PKT_PTS
FF_DISABLE_DEPRECATION_WARNINGS
        frame->pkt_pts = frame->pts;
FF_ENABLE_DEPRECATION_WARNINGS
#endif
        av_frame_set_pkt_pos(frame, -1);
        av_frame_set_pkt_duration(frame, 0);
        av_frame_set_pkt_size(frame, -1);

        frame->interlaced_frame = !parsed_frame.is_deinterlacing && !parsed_frame.dispinfo.progressive_frame;

        if (frame->interlaced_frame)
            frame->top_field_first = parsed_frame.dispinfo.top_field_first;

        if (parsed_frame.a53_caption)
        {
            AVFrameSideData *sd = av_frame_new_side_data(frame, AV_FRAME_DATA_A53_CC, parsed_frame.a53_caption_size);
            if (sd)
                memcpy(sd->data, parsed_frame.a53_caption, parsed_frame.a53_caption_size);
            av_freep(&parsed_frame.a53_caption);
            avctx->properties |= FF_CODEC_PROPERTY_CLOSED_CAPTIONS;
        }
    } else if (ctx->decoder_flushing) {
        ret = AVERROR_EOF;
    } else {
        ret = AVERROR(EAGAIN);
    }

error:
    if (mapped_frame)
        eret = CHECK_CU(ctx->cvdl->cuvidUnmapVideoFrame(ctx->cudecoder, mapped_frame));

    eret = CHECK_CU(ctx->cudl->cuCtxPopCurrent(&dummy));

    if (eret < 0)
        return eret;
    else
        return ret;
}

static int cuvid_decode_frame(AVCodecContext *avctx, void *data, int *got_frame, AVPacket *avpkt)
{
    CuvidContext *ctx = avctx->priv_data;
    AVFrame *frame = data;
    int ret = 0;

    av_log(avctx, AV_LOG_TRACE, "cuvid_decode_frame\n");

    if (ctx->deint_mode_current != cudaVideoDeinterlaceMode_Weave) {
        av_log(avctx, AV_LOG_ERROR, "Deinterlacing is not supported via the old API\n");
        return AVERROR(EINVAL);
    }

    if (!ctx->decoder_flushing) {
        ret = cuvid_decode_packet(avctx, avpkt);
        if (ret < 0)
            return ret;
    }

    ret = cuvid_output_frame(avctx, frame);
    if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF) {
        *got_frame = 0;
    } else if (ret < 0) {
        return ret;
    } else {
        *got_frame = 1;
    }

    return 0;
}

static av_cold int cuvid_decode_end(AVCodecContext *avctx)
{
    CuvidContext *ctx = avctx->priv_data;

    av_fifo_freep(&ctx->frame_queue);

    if (ctx->bsf)
        av_bsf_free(&ctx->bsf);

    if (ctx->cuparser)
        ctx->cvdl->cuvidDestroyVideoParser(ctx->cuparser);

    if (ctx->cudecoder)
        ctx->cvdl->cuvidDestroyDecoder(ctx->cudecoder);

    ctx->cudl = NULL;

    av_buffer_unref(&ctx->hwframe);
    av_buffer_unref(&ctx->hwdevice);

    cuvid_free_functions(&ctx->cvdl);

    return 0;
}

static int cuvid_test_dummy_decoder(AVCodecContext *avctx,
                                    const CUVIDPARSERPARAMS *cuparseinfo,
                                    int probed_width,
                                    int probed_height)
{
    CuvidContext *ctx = avctx->priv_data;
    CUVIDDECODECREATEINFO cuinfo;
    CUvideodecoder cudec = 0;
    int ret = 0;

    memset(&cuinfo, 0, sizeof(cuinfo));

    cuinfo.CodecType = cuparseinfo->CodecType;
    cuinfo.ChromaFormat = cudaVideoChromaFormat_420;
    cuinfo.OutputFormat = cudaVideoSurfaceFormat_NV12;

    cuinfo.ulWidth = probed_width;
    cuinfo.ulHeight = probed_height;
    cuinfo.ulTargetWidth = cuinfo.ulWidth;
    cuinfo.ulTargetHeight = cuinfo.ulHeight;

    cuinfo.target_rect.left = 0;
    cuinfo.target_rect.top = 0;
    cuinfo.target_rect.right = cuinfo.ulWidth;
    cuinfo.target_rect.bottom = cuinfo.ulHeight;

    cuinfo.ulNumDecodeSurfaces = ctx->nb_surfaces;
    cuinfo.ulNumOutputSurfaces = 1;
    cuinfo.ulCreationFlags = cudaVideoCreate_PreferCUVID;
    cuinfo.bitDepthMinus8 = 0;

    cuinfo.DeinterlaceMode = cudaVideoDeinterlaceMode_Weave;

    ret = CHECK_CU(ctx->cvdl->cuvidCreateDecoder(&cudec, &cuinfo));
    if (ret < 0)
        return ret;

    ret = CHECK_CU(ctx->cvdl->cuvidDestroyDecoder(cudec));
    if (ret < 0)
        return ret;

    return 0;
}

static av_cold int cuvid_decode_init(AVCodecContext *avctx)
{
    CuvidContext *ctx = avctx->priv_data;
    AVCUDADeviceContext *device_hwctx;
    AVHWDeviceContext *device_ctx;
    AVHWFramesContext *hwframe_ctx;
    CUVIDSOURCEDATAPACKET seq_pkt;
    CUcontext cuda_ctx = NULL;
    CUcontext dummy;
    const AVBitStreamFilter *bsf;
    int ret = 0;

    enum AVPixelFormat pix_fmts[3] = { AV_PIX_FMT_CUDA,
                                       AV_PIX_FMT_NV12,
                                       AV_PIX_FMT_NONE };

    int probed_width = avctx->coded_width ? avctx->coded_width : 1280;
    int probed_height = avctx->coded_height ? avctx->coded_height : 720;

    // Accelerated transcoding scenarios with 'ffmpeg' require that the
    // pix_fmt be set to AV_PIX_FMT_CUDA early. The sw_pix_fmt, and the
    // pix_fmt for non-accelerated transcoding, do not need to be correct
    // but need to be set to something. We arbitrarily pick NV12.
    ret = ff_get_format(avctx, pix_fmts);
    if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "ff_get_format failed: %d\n", ret);
        return ret;
    }
    avctx->pix_fmt = ret;

    ret = cuvid_load_functions(&ctx->cvdl);
    if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Failed loading nvcuvid.\n");
        goto error;
    }

    ctx->frame_queue = av_fifo_alloc(ctx->nb_surfaces * sizeof(CuvidParsedFrame));
    if (!ctx->frame_queue) {
        ret = AVERROR(ENOMEM);
        goto error;
    }

    if (avctx->hw_frames_ctx) {
        ctx->hwframe = av_buffer_ref(avctx->hw_frames_ctx);
        if (!ctx->hwframe) {
            ret = AVERROR(ENOMEM);
            goto error;
        }

        hwframe_ctx = (AVHWFramesContext*)ctx->hwframe->data;

        ctx->hwdevice = av_buffer_ref(hwframe_ctx->device_ref);
        if (!ctx->hwdevice) {
            ret = AVERROR(ENOMEM);
            goto error;
        }
    } else {
        ret = av_hwdevice_ctx_create(&ctx->hwdevice, AV_HWDEVICE_TYPE_CUDA, ctx->cu_gpu, NULL, 0);
        if (ret < 0)
            goto error;

        ctx->hwframe = av_hwframe_ctx_alloc(ctx->hwdevice);
        if (!ctx->hwframe) {
            av_log(avctx, AV_LOG_ERROR, "av_hwframe_ctx_alloc failed\n");
            ret = AVERROR(ENOMEM);
            goto error;
        }

        hwframe_ctx = (AVHWFramesContext*)ctx->hwframe->data;
    }

    device_ctx = hwframe_ctx->device_ctx;
    device_hwctx = device_ctx->hwctx;

    cuda_ctx = device_hwctx->cuda_ctx;
    ctx->cudl = device_hwctx->internal->cuda_dl;

    memset(&ctx->cuparseinfo, 0, sizeof(ctx->cuparseinfo));
    memset(&ctx->cuparse_ext, 0, sizeof(ctx->cuparse_ext));
    memset(&seq_pkt, 0, sizeof(seq_pkt));

    ctx->cuparseinfo.pExtVideoInfo = &ctx->cuparse_ext;

    switch (avctx->codec->id) {
#if CONFIG_H264_CUVID_DECODER
    case AV_CODEC_ID_H264:
        ctx->cuparseinfo.CodecType = cudaVideoCodec_H264;
        break;
#endif
#if CONFIG_HEVC_CUVID_DECODER
    case AV_CODEC_ID_HEVC:
        ctx->cuparseinfo.CodecType = cudaVideoCodec_HEVC;
        break;
#endif
#if CONFIG_MJPEG_CUVID_DECODER
    case AV_CODEC_ID_MJPEG:
        ctx->cuparseinfo.CodecType = cudaVideoCodec_JPEG;
        break;
#endif
#if CONFIG_MPEG1_CUVID_DECODER
    case AV_CODEC_ID_MPEG1VIDEO:
        ctx->cuparseinfo.CodecType = cudaVideoCodec_MPEG1;
        break;
#endif
#if CONFIG_MPEG2_CUVID_DECODER
    case AV_CODEC_ID_MPEG2VIDEO:
        ctx->cuparseinfo.CodecType = cudaVideoCodec_MPEG2;
        break;
#endif
#if CONFIG_MPEG4_CUVID_DECODER
    case AV_CODEC_ID_MPEG4:
        ctx->cuparseinfo.CodecType = cudaVideoCodec_MPEG4;
        break;
#endif
#if CONFIG_VP8_CUVID_DECODER
    case AV_CODEC_ID_VP8:
        ctx->cuparseinfo.CodecType = cudaVideoCodec_VP8;
        break;
#endif
#if CONFIG_VP9_CUVID_DECODER
    case AV_CODEC_ID_VP9:
        ctx->cuparseinfo.CodecType = cudaVideoCodec_VP9;
        break;
#endif
#if CONFIG_VC1_CUVID_DECODER
    case AV_CODEC_ID_VC1:
        ctx->cuparseinfo.CodecType = cudaVideoCodec_VC1;
        break;
#endif
    default:
        av_log(avctx, AV_LOG_ERROR, "Invalid CUVID codec!\n");
        return AVERROR_BUG;
    }

    if (avctx->codec->id == AV_CODEC_ID_H264 || avctx->codec->id == AV_CODEC_ID_HEVC) {
        if (avctx->codec->id == AV_CODEC_ID_H264)
            bsf = av_bsf_get_by_name("h264_mp4toannexb");
        else
            bsf = av_bsf_get_by_name("hevc_mp4toannexb");

        if (!bsf) {
            ret = AVERROR_BSF_NOT_FOUND;
            goto error;
        }
        if (ret = av_bsf_alloc(bsf, &ctx->bsf)) {
            goto error;
        }
        if (((ret = avcodec_parameters_from_context(ctx->bsf->par_in, avctx)) < 0) || ((ret = av_bsf_init(ctx->bsf)) < 0)) {
            av_bsf_free(&ctx->bsf);
            goto error;
        }

        ctx->cuparse_ext.format.seqhdr_data_length = ctx->bsf->par_out->extradata_size;
        memcpy(ctx->cuparse_ext.raw_seqhdr_data,
               ctx->bsf->par_out->extradata,
               FFMIN(sizeof(ctx->cuparse_ext.raw_seqhdr_data), ctx->bsf->par_out->extradata_size));
    } else if (avctx->extradata_size > 0) {
        ctx->cuparse_ext.format.seqhdr_data_length = avctx->extradata_size;
        memcpy(ctx->cuparse_ext.raw_seqhdr_data,
               avctx->extradata,
               FFMIN(sizeof(ctx->cuparse_ext.raw_seqhdr_data), avctx->extradata_size));
    }

    ctx->cuparseinfo.ulMaxNumDecodeSurfaces = ctx->nb_surfaces;
    ctx->cuparseinfo.ulMaxDisplayDelay = 4;
    ctx->cuparseinfo.pUserData = avctx;
    ctx->cuparseinfo.pfnSequenceCallback = cuvid_handle_video_sequence;
    ctx->cuparseinfo.pfnDecodePicture = cuvid_handle_picture_decode;
    ctx->cuparseinfo.pfnDisplayPicture = cuvid_handle_picture_display;

    ret = CHECK_CU(ctx->cudl->cuCtxPushCurrent(cuda_ctx));
    if (ret < 0)
        goto error;

    ret = cuvid_test_dummy_decoder(avctx, &ctx->cuparseinfo,
                                   probed_width,
                                   probed_height);
    if (ret < 0)
        goto error;

    ret = CHECK_CU(ctx->cvdl->cuvidCreateVideoParser(&ctx->cuparser, &ctx->cuparseinfo));
    if (ret < 0)
        goto error;

    seq_pkt.payload = ctx->cuparse_ext.raw_seqhdr_data;
    seq_pkt.payload_size = ctx->cuparse_ext.format.seqhdr_data_length;

    if (seq_pkt.payload && seq_pkt.payload_size) {
        ret = CHECK_CU(ctx->cvdl->cuvidParseVideoData(ctx->cuparser, &seq_pkt));
        if (ret < 0)
            goto error;
    }

    ret = CHECK_CU(ctx->cudl->cuCtxPopCurrent(&dummy));
    if (ret < 0)
        goto error;

    ctx->prev_pts = INT64_MIN;

    if (!avctx->pkt_timebase.num || !avctx->pkt_timebase.den)
        av_log(avctx, AV_LOG_WARNING, "Invalid pkt_timebase, passing timestamps as-is.\n");

    return 0;

error:
    cuvid_decode_end(avctx);
    return ret;
}

static void cuvid_flush(AVCodecContext *avctx)
{
    CuvidContext *ctx = avctx->priv_data;
    AVHWDeviceContext *device_ctx = (AVHWDeviceContext*)ctx->hwdevice->data;
    AVCUDADeviceContext *device_hwctx = device_ctx->hwctx;
    CUcontext dummy, cuda_ctx = device_hwctx->cuda_ctx;
    CUVIDSOURCEDATAPACKET seq_pkt = { 0 };
    int ret;

    ret = CHECK_CU(ctx->cudl->cuCtxPushCurrent(cuda_ctx));
    if (ret < 0)
        goto error;

    av_fifo_freep(&ctx->frame_queue);

    ctx->frame_queue = av_fifo_alloc(ctx->nb_surfaces * sizeof(CuvidParsedFrame));
    if (!ctx->frame_queue) {
        av_log(avctx, AV_LOG_ERROR, "Failed to recreate frame queue on flush\n");
        return;
    }

    if (ctx->cudecoder) {
        ctx->cvdl->cuvidDestroyDecoder(ctx->cudecoder);
        ctx->cudecoder = NULL;
    }

    if (ctx->cuparser) {
        ctx->cvdl->cuvidDestroyVideoParser(ctx->cuparser);
        ctx->cuparser = NULL;
    }

    ret = CHECK_CU(ctx->cvdl->cuvidCreateVideoParser(&ctx->cuparser, &ctx->cuparseinfo));
    if (ret < 0)
        goto error;

    seq_pkt.payload = ctx->cuparse_ext.raw_seqhdr_data;
    seq_pkt.payload_size = ctx->cuparse_ext.format.seqhdr_data_length;

    if (seq_pkt.payload && seq_pkt.payload_size) {
        ret = CHECK_CU(ctx->cvdl->cuvidParseVideoData(ctx->cuparser, &seq_pkt));
        if (ret < 0)
            goto error;
    }

    ret = CHECK_CU(ctx->cudl->cuCtxPopCurrent(&dummy));
    if (ret < 0)
        goto error;

    ctx->prev_pts = INT64_MIN;
    ctx->decoder_flushing = 0;

    return;
 error:
    av_log(avctx, AV_LOG_ERROR, "CUDA reinit on flush failed\n");
}

#define OFFSET(x) offsetof(CuvidContext, x)
#define VD AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_DECODING_PARAM
static const AVOption options[] = {
    { "deint",    "Set deinterlacing mode", OFFSET(deint_mode), AV_OPT_TYPE_INT,   { .i64 = cudaVideoDeinterlaceMode_Weave    }, cudaVideoDeinterlaceMode_Weave, cudaVideoDeinterlaceMode_Adaptive, VD, "deint" },
    { "weave",    "Weave deinterlacing (do nothing)",        0, AV_OPT_TYPE_CONST, { .i64 = cudaVideoDeinterlaceMode_Weave    }, 0, 0, VD, "deint" },
    { "bob",      "Bob deinterlacing",                       0, AV_OPT_TYPE_CONST, { .i64 = cudaVideoDeinterlaceMode_Bob      }, 0, 0, VD, "deint" },
    { "adaptive", "Adaptive deinterlacing",                  0, AV_OPT_TYPE_CONST, { .i64 = cudaVideoDeinterlaceMode_Adaptive }, 0, 0, VD, "deint" },
    { "gpu",      "GPU to be used for decoding", OFFSET(cu_gpu), AV_OPT_TYPE_STRING, { .str = NULL }, 0, 0, VD },
    { "surfaces", "Maximum surfaces to be used for decoding", OFFSET(nb_surfaces), AV_OPT_TYPE_INT, { .i64 = 25 }, 0, INT_MAX, VD },
    { "drop_second_field", "Drop second field when deinterlacing", OFFSET(drop_second_field), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, VD },
    { NULL }
};

#define DEFINE_CUVID_CODEC(x, X) \
    static const AVClass x##_cuvid_class = { \
        .class_name = #x "_cuvid", \
        .item_name = av_default_item_name, \
        .option = options, \
        .version = LIBAVUTIL_VERSION_INT, \
    }; \
    AVHWAccel ff_##x##_cuvid_hwaccel = { \
        .name           = #x "_cuvid", \
        .type           = AVMEDIA_TYPE_VIDEO, \
        .id             = AV_CODEC_ID_##X, \
        .pix_fmt        = AV_PIX_FMT_CUDA, \
    }; \
    AVCodec ff_##x##_cuvid_decoder = { \
        .name           = #x "_cuvid", \
        .long_name      = NULL_IF_CONFIG_SMALL("Nvidia CUVID " #X " decoder"), \
        .type           = AVMEDIA_TYPE_VIDEO, \
        .id             = AV_CODEC_ID_##X, \
        .priv_data_size = sizeof(CuvidContext), \
        .priv_class     = &x##_cuvid_class, \
        .init           = cuvid_decode_init, \
        .close          = cuvid_decode_end, \
        .decode         = cuvid_decode_frame, \
        .send_packet    = cuvid_decode_packet, \
        .receive_frame  = cuvid_output_frame, \
        .flush          = cuvid_flush, \
        .capabilities   = AV_CODEC_CAP_DELAY | AV_CODEC_CAP_AVOID_PROBING, \
        .pix_fmts       = (const enum AVPixelFormat[]){ AV_PIX_FMT_CUDA, \
                                                        AV_PIX_FMT_NV12, \
                                                        AV_PIX_FMT_P010, \
                                                        AV_PIX_FMT_P016, \
                                                        AV_PIX_FMT_NONE }, \
    };

#if CONFIG_HEVC_CUVID_DECODER
DEFINE_CUVID_CODEC(hevc, HEVC)
#endif

#if CONFIG_H264_CUVID_DECODER
//DEFINE_CUVID_CODEC(h264, H264)
#endif

#if CONFIG_MJPEG_CUVID_DECODER
DEFINE_CUVID_CODEC(mjpeg, MJPEG)
#endif

#if CONFIG_MPEG1_CUVID_DECODER
DEFINE_CUVID_CODEC(mpeg1, MPEG1VIDEO)
#endif

#if CONFIG_MPEG2_CUVID_DECODER
//DEFINE_CUVID_CODEC(mpeg2, MPEG2VIDEO)
#endif

#if CONFIG_MPEG4_CUVID_DECODER
DEFINE_CUVID_CODEC(mpeg4, MPEG4)
#endif

#if CONFIG_VP8_CUVID_DECODER
DEFINE_CUVID_CODEC(vp8, VP8)
#endif

#if CONFIG_VP9_CUVID_DECODER
DEFINE_CUVID_CODEC(vp9, VP9)
#endif

#if CONFIG_VC1_CUVID_DECODER
DEFINE_CUVID_CODEC(vc1, VC1)
#endif


static int map_avcodec_id(enum AVCodecID id)
{
    switch (id) {
    case AV_CODEC_ID_H264: return cudaVideoCodec_H264;
	case AV_CODEC_ID_MPEG2VIDEO: return cudaVideoCodec_MPEG2;
    }
    return -1;
}

static int map_chroma_format(enum AVPixelFormat pix_fmt)
{
    int shift_h = 0, shift_v = 0;

    av_pix_fmt_get_chroma_sub_sample(pix_fmt, &shift_h, &shift_v);

    if (shift_h == 1 && shift_v == 1)
        return cudaVideoChromaFormat_420;
    else if (shift_h == 1 && shift_v == 0)
        return cudaVideoChromaFormat_422;
    else if (shift_h == 0 && shift_v == 0)
        return cudaVideoChromaFormat_444;

    return -1;
}

static int ff_cuvid_decode_init(AVCodecContext *avctx)
{
    CUVIDContext *ctx = avctx->internal->hwaccel_priv_data;

    AVHWFramesContext   *frames_ctx;
    AVCUDADeviceContext *device_hwctx;

    CUVIDDECODECREATEINFO params = { 0 };
    CUcontext dummy;
    CUresult err;

    int cuvid_codec_type, cuvid_chroma_format;
    int ret = 0;

    cuvid_codec_type = map_avcodec_id(avctx->codec_id);
    if (cuvid_codec_type < 0) {
        av_log(avctx, AV_LOG_ERROR, "Unsupported codec ID\n");
        return AVERROR_BUG;
    }

    cuvid_chroma_format = map_chroma_format(avctx->sw_pix_fmt);
    if (cuvid_chroma_format < 0) {
        av_log(avctx, AV_LOG_ERROR, "Unsupported chroma format\n");
        return AVERROR(ENOSYS);
    }

    if (!avctx->hw_frames_ctx) {
        av_log(avctx, AV_LOG_ERROR, "A hardware frames context is "
               "required for CUVID decoding.\n");
        return AVERROR(EINVAL);
    }

    frames_ctx   = (AVHWFramesContext*)avctx->hw_frames_ctx->data;
    device_hwctx = frames_ctx->device_ctx->hwctx;

    ctx->cudl = device_hwctx->internal->cuda_dl;
    ctx->cuda_ctx = device_hwctx->cuda_ctx;

    ret = cuvid_load_functions(&ctx->cvdl);
    if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Failed loading nvcuvid.\n");
        goto finish;
    }

    params.ulWidth             = avctx->coded_width;
    params.ulHeight            = avctx->coded_height;
    params.ulTargetWidth       = avctx->coded_width;
    params.ulTargetHeight      = avctx->coded_height;
    params.OutputFormat        = cudaVideoSurfaceFormat_NV12;
    params.CodecType           = cuvid_codec_type;
    params.ChromaFormat        = cuvid_chroma_format;
    params.ulNumDecodeSurfaces = 32;
    params.ulNumOutputSurfaces = 1;
	params.DeinterlaceMode = avctx->field_order <= AV_FIELD_PROGRESSIVE ? cudaVideoDeinterlaceMode_Weave : cudaVideoDeinterlaceMode_Bob;

    err = ctx->cudl->cuCtxPushCurrent(ctx->cuda_ctx);
    if (err != CUDA_SUCCESS)
        return AVERROR_UNKNOWN;

    err = ctx->cvdl->cuvidCreateDecoder(&ctx->decoder, &params);
    if (err != CUDA_SUCCESS) {
        ret = AVERROR_UNKNOWN;
        goto finish;
    }

	avctx->field_order = AV_FIELD_PROGRESSIVE;
	avctx->flags &= ~AV_CODEC_FLAG_INTERLACED_DCT;

finish:
    ctx->cudl->cuCtxPopCurrent(&dummy);

    return ret;
}

static int ff_cuvid_decode_uninit(AVCodecContext *avctx)
{
    CUVIDContext *ctx = avctx->internal->hwaccel_priv_data;

    av_freep(&ctx->bitstream);
    ctx->bitstream_len       = 0;
    ctx->bitstream_allocated = 0;

    av_freep(&ctx->slice_offsets);
    ctx->nb_slices               = 0;
    ctx->slice_offsets_allocated = 0;

    if (ctx->decoder)
        ctx->cvdl->cuvidDestroyDecoder(ctx->decoder);
    ctx->decoder = NULL;

    cuvid_free_functions(&ctx->cvdl);


    return 0;
}

static int ff_cuvid_start_frame(AVCodecContext *avctx)
{
    CUVIDContext *ctx = avctx->internal->hwaccel_priv_data;

    ctx->bitstream_len = 0;
    ctx->nb_slices     = 0;

    return 0;
}

static int ff_cuvid_end_frame(AVCodecContext *avctx, CUVIDFrame *frame)
{
	MpegEncContext * const s = avctx->priv_data;
	CUVIDContext  *ctx = avctx->internal->hwaccel_priv_data;
    CUVIDPICPARAMS *pp = &ctx->pic_params;

    CUVIDPROCPARAMS vpp = { 0 };
    CUresult err;
    CUcontext dummy;
    CUdeviceptr devptr;

	if (frame->f->interlaced_frame)
	{
		vpp.second_field = !s->first_field;
		vpp.top_field_first = frame->f->top_field_first;
	}
	else
	{
		vpp.progressive_frame = 1;
	}
	frame->f->interlaced_frame = 0;

    unsigned int pitch, i;
    unsigned int offset = 0;
    int ret = 0;

    pp->nBitstreamDataLen = ctx->bitstream_len;
    pp->pBitstreamData    = ctx->bitstream;
    pp->nNumSlices        = ctx->nb_slices;
    pp->pSliceDataOffsets = ctx->slice_offsets;

    err = ctx->cudl->cuCtxPushCurrent(ctx->cuda_ctx);
    if (err != CUDA_SUCCESS)
        return AVERROR_UNKNOWN;

    err = ctx->cvdl->cuvidDecodePicture(ctx->decoder, &ctx->pic_params);
    if (err != CUDA_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Error decoding a picture with CUVID: %d\n",
               err);
        ret = AVERROR_UNKNOWN;
        goto finish;
    }

    if (pp->field_pic_flag && !pp->second_field)
        goto finish;

    err = ctx->cvdl->cuvidMapVideoFrame(ctx->decoder, frame->idx, &devptr, &pitch, &vpp);
    if (err != CUDA_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Error mapping a picture with CUVID: %d\n",
               err);
        ret = AVERROR_UNKNOWN;
        goto finish;
    }

    for (i = 0; frame->f->data[i]; i++) {
        CUDA_MEMCPY2D cpy = {
            .srcMemoryType = CU_MEMORYTYPE_DEVICE,
            .dstMemoryType = CU_MEMORYTYPE_DEVICE,
            .srcDevice     = devptr,
            .dstDevice     = (CUdeviceptr)frame->f->data[i],
            .srcPitch      = pitch,
            .dstPitch      = frame->f->linesize[i],
            .srcY          = offset,
            .WidthInBytes  = FFMIN(pitch, frame->f->linesize[i]),
            .Height        = avctx->coded_height >> (i ? 1 : 0),
        };

        err = ctx->cudl->cuMemcpy2D(&cpy);
        if (err != CUDA_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR, "Error copying decoded frame: %d\n",
                   err);
            ret = AVERROR_UNKNOWN;
            goto copy_fail;
        }

        offset += cpy.Height;
    }

copy_fail:
    ctx->cvdl->cuvidUnmapVideoFrame(ctx->decoder, devptr);

finish:
    ctx->cudl->cuCtxPopCurrent(&dummy);
    return ret;
}







static void dpb_add(const H264Context *h, CUVIDH264DPBENTRY *dst, const H264Picture *src,
                    int frame_idx)
{
    const CUVIDFrame *frame = src->hwaccel_picture_private;

    dst->PicIdx             = frame->idx;
    dst->FrameIdx           = frame_idx;
    dst->is_long_term       = src->long_ref;
    dst->not_existing       = 0;
    dst->used_for_reference = src->reference & 3;
    dst->FieldOrderCnt[0]   = src->field_poc[0];
    dst->FieldOrderCnt[1]   = src->field_poc[1];
}

static int cuvid_h264_start_frame(AVCodecContext *avctx,
                                  const uint8_t *buffer, uint32_t size)
{
    const H264Context *h = avctx->priv_data;
    const PPS *pps = h->ps.pps;
    const SPS *sps = h->ps.sps;

    CUVIDContext       *ctx = avctx->internal->hwaccel_priv_data;
    CUVIDPICPARAMS      *pp = &ctx->pic_params;
    CUVIDH264PICPARAMS *ppc = &pp->CodecSpecific.h264;
    CUVIDFrame       *frame = h->cur_pic_ptr->hwaccel_picture_private;

    int i, dpb_size;

    frame->idx = h->cur_pic_ptr - h->DPB;
    frame->f   = h->cur_pic_ptr->f;

    *pp = (CUVIDPICPARAMS) {
        .PicWidthInMbs     = h->mb_width,
        .FrameHeightInMbs  = h->mb_height,
        .CurrPicIdx        = frame->idx,
        .field_pic_flag    = FIELD_PICTURE(h),
        .bottom_field_flag = h->picture_structure == PICT_BOTTOM_FIELD,
        .second_field      = FIELD_PICTURE(h) && !h->first_field,
        .ref_pic_flag      = h->nal_ref_idc != 0,
        .intra_pic_flag    = 0,

        .CodecSpecific.h264 = {
            .log2_max_frame_num_minus4            = sps->log2_max_frame_num - 4,
            .pic_order_cnt_type                   = sps->poc_type,
            .log2_max_pic_order_cnt_lsb_minus4    = FFMAX(sps->log2_max_poc_lsb - 4, 0),
            .delta_pic_order_always_zero_flag     = sps->delta_pic_order_always_zero_flag,
            .frame_mbs_only_flag                  = sps->frame_mbs_only_flag,
            .direct_8x8_inference_flag            = sps->direct_8x8_inference_flag,
            .num_ref_frames                       = sps->ref_frame_count,
            .residual_colour_transform_flag       = sps->residual_color_transform_flag,
            .bit_depth_luma_minus8                = sps->bit_depth_luma - 8,
            .bit_depth_chroma_minus8              = sps->bit_depth_chroma - 8,
            .qpprime_y_zero_transform_bypass_flag = sps->transform_bypass,

            .entropy_coding_mode_flag               = pps->cabac,
            .pic_order_present_flag                 = pps->pic_order_present,
            .num_ref_idx_l0_active_minus1           = pps->ref_count[0] - 1,
            .num_ref_idx_l1_active_minus1           = pps->ref_count[1] - 1,
            .weighted_pred_flag                     = pps->weighted_pred,
            .weighted_bipred_idc                    = pps->weighted_bipred_idc,
            .pic_init_qp_minus26                    = pps->init_qp - 26,
            .deblocking_filter_control_present_flag = pps->deblocking_filter_parameters_present,
            .redundant_pic_cnt_present_flag         = pps->redundant_pic_cnt_present,
            .transform_8x8_mode_flag                = pps->transform_8x8_mode,
            .MbaffFrameFlag                         = sps->mb_aff && !FIELD_PICTURE(h),
            .constrained_intra_pred_flag            = pps->constrained_intra_pred,
            .chroma_qp_index_offset                 = pps->chroma_qp_index_offset[0],
            .second_chroma_qp_index_offset          = pps->chroma_qp_index_offset[1],
            .ref_pic_flag                           = h->nal_ref_idc != 0,
            .frame_num                              = h->poc.frame_num,
            .CurrFieldOrderCnt[0]                   = h->cur_pic_ptr->field_poc[0],
            .CurrFieldOrderCnt[1]                   = h->cur_pic_ptr->field_poc[1],
        },
    };

    memcpy(ppc->WeightScale4x4,    pps->scaling_matrix4,    sizeof(ppc->WeightScale4x4));
    memcpy(ppc->WeightScale8x8[0], pps->scaling_matrix8[0], sizeof(ppc->WeightScale8x8[0]));
    memcpy(ppc->WeightScale8x8[1], pps->scaling_matrix8[3], sizeof(ppc->WeightScale8x8[0]));

    dpb_size = 0;
    for (i = 0; i < h->short_ref_count; i++)
        dpb_add(h, &ppc->dpb[dpb_size++], h->short_ref[i], h->short_ref[i]->frame_num);
    for (i = 0; i < 16; i++) {
        if (h->long_ref[i])
            dpb_add(h, &ppc->dpb[dpb_size++], h->long_ref[i], i);
    }

    for (i = dpb_size; i < FF_ARRAY_ELEMS(ppc->dpb); i++)
        ppc->dpb[i].PicIdx       = -1;

    return ff_cuvid_start_frame(avctx);
}

static int cuvid_h264_decode_slice(AVCodecContext *avctx, const uint8_t *buffer,
                                   uint32_t size)
{
    CUVIDContext *ctx = avctx->internal->hwaccel_priv_data;
    void *tmp;

    tmp = av_fast_realloc(ctx->bitstream, &ctx->bitstream_allocated,
                          ctx->bitstream_len + size + 3);
    if (!tmp)
        return AVERROR(ENOMEM);
    ctx->bitstream = tmp;

    tmp = av_fast_realloc(ctx->slice_offsets, &ctx->slice_offsets_allocated,
                          (ctx->nb_slices + 1) * sizeof(*ctx->slice_offsets));
    if (!tmp)
        return AVERROR(ENOMEM);
    ctx->slice_offsets = tmp;

    AV_WB24(ctx->bitstream + ctx->bitstream_len, 1);
    memcpy(ctx->bitstream + ctx->bitstream_len + 3, buffer, size);
    ctx->slice_offsets[ctx->nb_slices] = ctx->bitstream_len ;
    ctx->bitstream_len += size + 3;
    ctx->nb_slices++;

    return 0;
}

static int cuvid_h264_end_frame(AVCodecContext *avctx)
{
    H264Context *h = avctx->priv_data;
    return ff_cuvid_end_frame(avctx, h->cur_pic_ptr->hwaccel_picture_private);
}

AVHWAccel ff_h264_cuvid_hwaccel = {
    .name                 = "h264_cuvid",
    .type                 = AVMEDIA_TYPE_VIDEO,
    .id                   = AV_CODEC_ID_H264,
    .pix_fmt              = AV_PIX_FMT_CUDA,
    .start_frame          = cuvid_h264_start_frame,
    .end_frame            = cuvid_h264_end_frame,
    .decode_slice         = cuvid_h264_decode_slice,
    .init                 = ff_cuvid_decode_init,
    .uninit               = ff_cuvid_decode_uninit,
    .priv_data_size       = sizeof(CUVIDContext),
    .frame_priv_data_size = sizeof(CUVIDFrame),
};

static int cuvid_mpeg2_start_frame(AVCodecContext *avctx, const uint8_t *buffer, uint32_t size)
{
    MpegEncContext * const s = avctx->priv_data;
    Picture *pic             = s->current_picture_ptr;

    CUVIDContext       *ctx = avctx->internal->hwaccel_priv_data;
    CUVIDPICPARAMS      *pp = &ctx->pic_params;
    CUVIDMPEG2PICPARAMS *ppc = &pp->CodecSpecific.mpeg2;
    CUVIDFrame       *frame = s->current_picture_ptr->hwaccel_picture_private;

    int i;

    frame->idx = pic - s->picture;
    frame->f   = pic->f;

    *pp = (CUVIDPICPARAMS) {
        .PicWidthInMbs     = s->mb_width,
        .FrameHeightInMbs  = s->mb_height,
        .CurrPicIdx        = frame->idx,
        .field_pic_flag    = s->picture_structure != PICT_FRAME,
        .bottom_field_flag = s->picture_structure == PICT_BOTTOM_FIELD,
        .second_field      = s->picture_structure != PICT_FRAME && !s->first_field,
        .ref_pic_flag      = pic->reference != 0,
        .intra_pic_flag    = 0,

        .CodecSpecific.mpeg2 = {
	    .ForwardRefIdx = s->pict_type == AV_PICTURE_TYPE_B || s->pict_type == AV_PICTURE_TYPE_P ? s->last_picture_ptr - s->picture : -1,
	    .BackwardRefIdx = s->pict_type == AV_PICTURE_TYPE_B ? s->next_picture_ptr - s->picture : -1,
	    .picture_coding_type = s->pict_type,
	    .full_pel_forward_vector = s->full_pel[0],
	    .full_pel_backward_vector = s->full_pel[1],
	    .intra_dc_precision = s->intra_dc_precision,
	    .frame_pred_frame_dct = s->frame_pred_frame_dct,
	    .concealment_motion_vectors = s->concealment_motion_vectors,
	    .q_scale_type = s->q_scale_type,
	    .intra_vlc_format = s->intra_vlc_format,
	    .alternate_scan = s->alternate_scan,
	    .top_field_first = s->top_field_first,
	}
    };

    ppc->f_code[0][0]               = s->mpeg_f_code[0][0];
    ppc->f_code[0][1]               = s->mpeg_f_code[0][1];
    ppc->f_code[1][0]               = s->mpeg_f_code[1][0];
    ppc->f_code[1][1]               = s->mpeg_f_code[1][1];

    for (i = 0; i < 64; ++i) {
        ppc->QuantMatrixIntra[i] = s->intra_matrix[i];
        ppc->QuantMatrixInter[i] = s->inter_matrix[i];
    }

    return ff_cuvid_start_frame(avctx);
}

static int cuvid_mpeg2_decode_slice(AVCodecContext *avctx, const uint8_t *buffer,
                                   uint32_t size)
{
    CUVIDContext *ctx = avctx->internal->hwaccel_priv_data;
    void *tmp;

    tmp = av_fast_realloc(ctx->bitstream, &ctx->bitstream_allocated,
                          ctx->bitstream_len + size);
    if (!tmp)
        return AVERROR(ENOMEM);
    ctx->bitstream = tmp;

    tmp = av_fast_realloc(ctx->slice_offsets, &ctx->slice_offsets_allocated,
                          (ctx->nb_slices + 1) * sizeof(*ctx->slice_offsets));
    if (!tmp)
        return AVERROR(ENOMEM);
    ctx->slice_offsets = tmp;

    memcpy(ctx->bitstream + ctx->bitstream_len, buffer, size);
    ctx->slice_offsets[ctx->nb_slices] = ctx->bitstream_len ;
    ctx->bitstream_len += size;
    ctx->nb_slices++;

    return 0;
}

static int cuvid_mpeg2_end_frame(AVCodecContext *avctx)
{
	MpegEncContext * const s = avctx->priv_data;
	return ff_cuvid_end_frame(avctx, s->current_picture_ptr->hwaccel_picture_private);
}

AVHWAccel ff_mpeg2_cuvid_hwaccel = {
    .name                 = "mpeg2_cuvid",
    .type                 = AVMEDIA_TYPE_VIDEO,
    .id                   = AV_CODEC_ID_MPEG2VIDEO,
    .pix_fmt              = AV_PIX_FMT_CUDA,
    .start_frame          = cuvid_mpeg2_start_frame,
    .end_frame            = cuvid_mpeg2_end_frame,
    .decode_slice         = cuvid_mpeg2_decode_slice,
    .init                 = ff_cuvid_decode_init,
    .uninit               = ff_cuvid_decode_uninit,
    .priv_data_size       = sizeof(CUVIDContext),
    .frame_priv_data_size = sizeof(CUVIDFrame),
};
