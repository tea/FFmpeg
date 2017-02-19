/*
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

#include "libavutil/hwcontext.h"

#include "ffmpeg.h"

typedef struct CUVIDContext {
    AVBufferRef *hw_frames_ctx;
} CUVIDContext;

static int cuvid_get_buffer(AVCodecContext *s, AVFrame *frame, int flags)
{
	InputStream *ist = s->opaque;

	return av_hwframe_get_buffer(ist->hw_frames_ctx, frame, 0);
}

static void cuvid_uninit(AVCodecContext *avctx)
{
    InputStream  *ist = avctx->opaque;

    av_buffer_unref(&ist->hw_frames_ctx);

    ist->hwaccel_ctx = 0;
    ist->hwaccel_uninit = 0;
}

int cuvid_init(AVCodecContext *avctx)
{
    InputStream  *ist = avctx->opaque;

    av_log(NULL, AV_LOG_TRACE, "Initializing cuvid hwaccel\n");

	if (!ist->hw_frames_ctx)
	{
		av_log(NULL, AV_LOG_ERROR, "hw_frames_ctx is not set on CUVID input");
		return -1;
	}
    return 0;
}

int cuvid_transcode_init(OutputStream *ost)
{
	InputStream *ist = NULL;
	const enum AVPixelFormat *pix_fmt;
	AVHWFramesContext *encode_frames;
	AVBufferRef *encode_frames_ref = NULL;
	int err = 0;

	if (!ost->enc->pix_fmts)
		goto error;
	for (pix_fmt = ost->enc->pix_fmts; *pix_fmt != AV_PIX_FMT_NONE; pix_fmt++)
		if (*pix_fmt == AV_PIX_FMT_CUDA)
			break;
	if (*pix_fmt == AV_PIX_FMT_NONE)
		goto error;

	if (ost->source_index < 0)
		goto error;

	ist = input_streams[ost->source_index];
	if (ist->dec && ist->dec->pix_fmts)
	{
		for (pix_fmt = ist->dec->pix_fmts; *pix_fmt != AV_PIX_FMT_NONE; pix_fmt++)
			if (*pix_fmt == AV_PIX_FMT_CUDA)
				break;
		if (*pix_fmt == AV_PIX_FMT_NONE)
			goto error;
	}

	if (!hw_device_ctx) {
		err = av_hwdevice_ctx_create(&hw_device_ctx, AV_HWDEVICE_TYPE_CUDA, ist->hwaccel_device, NULL, 0);
		if (err < 0)
			goto error;
	}

	encode_frames_ref = av_hwframe_ctx_alloc(hw_device_ctx);
	if (!encode_frames_ref) {
		err = AVERROR(ENOMEM);
		goto error;
	}
	encode_frames = (AVHWFramesContext*)encode_frames_ref->data;

	encode_frames->width = FFALIGN(ist->resample_width, 32);
	encode_frames->height = FFALIGN(ist->resample_height, 32);
	encode_frames->format = AV_PIX_FMT_CUDA;
	encode_frames->sw_format = AV_PIX_FMT_NV12;
	encode_frames->initial_pool_size = 1;

	err = av_hwframe_ctx_init(encode_frames_ref);
	if (err < 0)
		goto error;

	ist->dec_ctx->pix_fmt = AV_PIX_FMT_CUDA;
	ist->resample_pix_fmt = AV_PIX_FMT_CUDA;

	ost->enc_ctx->pix_fmt = AV_PIX_FMT_CUDA;
	ost->enc_ctx->hw_frames_ctx = encode_frames_ref;

	ist->hwaccel_get_buffer = cuvid_get_buffer;
	ist->hwaccel_uninit = cuvid_uninit;
	ist->hw_frames_ctx = av_buffer_ref(encode_frames_ref);

	return 0;

error:
	if (ist && ist->hwaccel_id == HWACCEL_CUVID) {
		av_log(NULL, AV_LOG_ERROR, "CUVID decoding requested but GPU transcoding init failed\n");
		if (!err)
			err = -1;
	}

    return err;
}
