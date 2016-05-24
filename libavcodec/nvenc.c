/*
 * H.264 hardware encoding using nvidia nvenc
 * Copyright (c) 2014 Timo Rothenpieler <timo@rothenpieler.org>
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

#include "config.h"

#include <nvEncodeAPI.h>

#if defined(_WIN32)
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "libavutil/internal.h"
#include "libavutil/imgutils.h"
#include "libavutil/avassert.h"
#include "libavutil/mem.h"
#include "libavutil/hwcontext.h"
#include "internal.h"
#include "thread.h"
#include "nvenc.h"

#if CONFIG_CUDA
#include "libavutil/hwcontext_cuda.h"
#endif

#if defined(_WIN32)
#define LOAD_FUNC(l, s) GetProcAddress(l, s)
#define DL_CLOSE_FUNC(l) FreeLibrary(l)
#else
#define LOAD_FUNC(l, s) dlsym(l, s)
#define DL_CLOSE_FUNC(l) dlclose(l)
#endif

typedef struct NvencData
{
    union {
        int64_t timestamp;
        NVENCFrame *surface;
    } u;
} NvencData;

const enum AVPixelFormat ff_nvenc_pix_fmts[] = {
    AV_PIX_FMT_YUV420P,
    AV_PIX_FMT_NV12,
    AV_PIX_FMT_YUV444P,
#if CONFIG_CUDA
    AV_PIX_FMT_CUDA,
#endif
    AV_PIX_FMT_NONE
};

static const struct {
    NVENCSTATUS nverr;
    int         averr;
    const char *desc;
} nvenc_errors[] = {
    { NV_ENC_SUCCESS,                      0,                "success"                  },
    { NV_ENC_ERR_NO_ENCODE_DEVICE,         AVERROR(ENOENT),  "no encode device"         },
    { NV_ENC_ERR_UNSUPPORTED_DEVICE,       AVERROR(ENOSYS),  "unsupported device"       },
    { NV_ENC_ERR_INVALID_ENCODERDEVICE,    AVERROR(EINVAL),  "invalid encoder device"   },
    { NV_ENC_ERR_INVALID_DEVICE,           AVERROR(EINVAL),  "invalid device"           },
    { NV_ENC_ERR_DEVICE_NOT_EXIST,         AVERROR(EIO),     "device does not exist"    },
    { NV_ENC_ERR_INVALID_PTR,              AVERROR(EFAULT),  "invalid ptr"              },
    { NV_ENC_ERR_INVALID_EVENT,            AVERROR(EINVAL),  "invalid event"            },
    { NV_ENC_ERR_INVALID_PARAM,            AVERROR(EINVAL),  "invalid param"            },
    { NV_ENC_ERR_INVALID_CALL,             AVERROR(EINVAL),  "invalid call"             },
    { NV_ENC_ERR_OUT_OF_MEMORY,            AVERROR(ENOMEM),  "out of memory"            },
    { NV_ENC_ERR_ENCODER_NOT_INITIALIZED,  AVERROR(EINVAL),  "encoder not initialized"  },
    { NV_ENC_ERR_UNSUPPORTED_PARAM,        AVERROR(ENOSYS),  "unsupported param"        },
    { NV_ENC_ERR_LOCK_BUSY,                AVERROR(EAGAIN),  "lock busy"                },
    { NV_ENC_ERR_NOT_ENOUGH_BUFFER,        AVERROR(ENOBUFS), "not enough buffer"        },
    { NV_ENC_ERR_INVALID_VERSION,          AVERROR(EINVAL),  "invalid version"          },
    { NV_ENC_ERR_MAP_FAILED,               AVERROR(EIO),     "map failed"               },
    { NV_ENC_ERR_NEED_MORE_INPUT,          AVERROR(EAGAIN),  "need more input"          },
    { NV_ENC_ERR_ENCODER_BUSY,             AVERROR(EAGAIN),  "encoder busy"             },
    { NV_ENC_ERR_EVENT_NOT_REGISTERD,      AVERROR(EBADF),   "event not registered"     },
    { NV_ENC_ERR_GENERIC,                  AVERROR_UNKNOWN,  "generic error"            },
    { NV_ENC_ERR_INCOMPATIBLE_CLIENT_KEY,  AVERROR(EINVAL),  "incompatible client key"  },
    { NV_ENC_ERR_UNIMPLEMENTED,            AVERROR(ENOSYS),  "unimplemented"            },
    { NV_ENC_ERR_RESOURCE_REGISTER_FAILED, AVERROR(EIO),     "resource register failed" },
    { NV_ENC_ERR_RESOURCE_NOT_REGISTERED,  AVERROR(EBADF),   "resource not registered"  },
    { NV_ENC_ERR_RESOURCE_NOT_MAPPED,      AVERROR(EBADF),   "resource not mapped"      },
};

static int nvenc_map_error(NVENCSTATUS err, const char **desc)
{
    int i;
    for (i = 0; i < FF_ARRAY_ELEMS(nvenc_errors); i++) {
        if (nvenc_errors[i].nverr == err) {
            if (desc)
                *desc = nvenc_errors[i].desc;
            return nvenc_errors[i].averr;
        }
    }
    if (desc)
        *desc = "unknown error";
    return AVERROR_UNKNOWN;
}

static int nvenc_print_error(void *log_ctx, NVENCSTATUS err,
                             const char *error_string)
{
    const char *desc;
    int ret;
    ret = nvenc_map_error(err, &desc);
    av_log(log_ctx, AV_LOG_ERROR, "%s: %s (%d)\n", error_string, desc, err);
    return ret;
}

#define CHECK_LOAD_FUNC(t, f, s) \
do { \
    (f) = (t)LOAD_FUNC(nvel->cuda_lib, s); \
    if (!(f)) { \
        av_log(avctx, AV_LOG_FATAL, "Failed loading %s from CUDA library\n", s); \
        goto error; \
    } \
} while (0)

static av_cold int nvenc_dyload_cuda(AVCodecContext *avctx)
{
    NVENCContext *ctx         = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;

#if CONFIG_CUDA
    nvel->cu_init                      = cuInit;
    nvel->cu_device_get_count          = cuDeviceGetCount;
    nvel->cu_device_get                = cuDeviceGet;
    nvel->cu_device_get_name           = cuDeviceGetName;
    nvel->cu_device_compute_capability = cuDeviceComputeCapability;
    nvel->cu_ctx_create                = cuCtxCreate_v2;
    nvel->cu_ctx_pop_current           = cuCtxPopCurrent_v2;
    nvel->cu_ctx_destroy               = cuCtxDestroy_v2;

    return 1;
#else
    if (nvel->cuda_lib)
        return 1;

#if defined(_WIN32)
    nvel->cuda_lib = LoadLibrary(TEXT("nvcuda.dll"));
#else
    nvel->cuda_lib = dlopen("libcuda.so", RTLD_LAZY);
#endif

    if (!nvel->cuda_lib) {
        av_log(avctx, AV_LOG_FATAL, "Failed loading CUDA library\n");
        goto error;
    }

    CHECK_LOAD_FUNC(PCUINIT, nvel->cu_init, "cuInit");
    CHECK_LOAD_FUNC(PCUDEVICEGETCOUNT, nvel->cu_device_get_count, "cuDeviceGetCount");
    CHECK_LOAD_FUNC(PCUDEVICEGET, nvel->cu_device_get, "cuDeviceGet");
    CHECK_LOAD_FUNC(PCUDEVICEGETNAME, nvel->cu_device_get_name, "cuDeviceGetName");
    CHECK_LOAD_FUNC(PCUDEVICECOMPUTECAPABILITY, nvel->cu_device_compute_capability, "cuDeviceComputeCapability");
    CHECK_LOAD_FUNC(PCUCTXCREATE, nvel->cu_ctx_create, "cuCtxCreate_v2");
    CHECK_LOAD_FUNC(PCUCTXPOPCURRENT, nvel->cu_ctx_pop_current, "cuCtxPopCurrent_v2");
    CHECK_LOAD_FUNC(PCUCTXDESTROY, nvel->cu_ctx_destroy, "cuCtxDestroy_v2");

    return 1;

error:

    if (nvel->cuda_lib)
        DL_CLOSE_FUNC(nvel->cuda_lib);

    nvel->cuda_lib = NULL;

    return 0;
#endif
}

static av_cold int nvenc_open_session(AVCodecContext *avctx)
{
    NV_ENC_OPEN_ENCODE_SESSION_EX_PARAMS params = { 0 };
    NVENCContext *ctx                           = avctx->priv_data;
    NV_ENCODE_API_FUNCTION_LIST *nv             = &ctx->nvel.nvenc_funcs;
    NVENCSTATUS ret;

    params.version    = NV_ENC_OPEN_ENCODE_SESSION_EX_PARAMS_VER;
    params.apiVersion = NVENCAPI_VERSION;
    params.device     = ctx->cu_context;
    params.deviceType = NV_ENC_DEVICE_TYPE_CUDA;

    ret = nv->nvEncOpenEncodeSessionEx(&params, &ctx->nvenc_ctx);
    if (ret != NV_ENC_SUCCESS) {
        ctx->nvenc_ctx = NULL;
        return nvenc_print_error(avctx, ret, "OpenEncodeSessionEx failed");
    }

    return 0;
}

static av_cold int check_cuda_errors(AVCodecContext *avctx, CUresult err, const char *func)
{
    if (err != CUDA_SUCCESS) {
        av_log(avctx, AV_LOG_FATAL, ">> %s - failed with error code 0x%x\n", func, err);
        return 0;
    }
    return 1;
}
#define check_cuda_errors(f) if (!check_cuda_errors(avctx, f, #f)) goto error

static av_cold int nvenc_check_cuda(AVCodecContext *avctx)
{
    int device_count = 0;
    CUdevice cu_device = 0;
    char gpu_name[128];
    int smminor = 0, smmajor = 0;
    int i, smver, target_smver;

    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;

    switch (avctx->codec->id) {
    case AV_CODEC_ID_H264:
        target_smver = ctx->data_pix_fmt == AV_PIX_FMT_YUV444P ? 0x52 : 0x30;
        break;
    case AV_CODEC_ID_H265:
        target_smver = 0x52;
        break;
    default:
        av_log(avctx, AV_LOG_FATAL, "Unknown codec name\n");
        goto error;
    }

    if (!nvenc_dyload_cuda(avctx))
        return 0;

    if (nvel->nvenc_device_count > 0)
        return 1;

    check_cuda_errors(nvel->cu_init(0));

    check_cuda_errors(nvel->cu_device_get_count(&device_count));

    if (!device_count) {
        av_log(avctx, AV_LOG_FATAL, "No CUDA capable devices found\n");
        goto error;
    }

    av_log(avctx, AV_LOG_VERBOSE, "%d CUDA capable devices found\n", device_count);

    nvel->nvenc_device_count = 0;

    for (i = 0; i < device_count; ++i) {
        check_cuda_errors(nvel->cu_device_get(&cu_device, i));
        check_cuda_errors(nvel->cu_device_get_name(gpu_name, sizeof(gpu_name), cu_device));
        check_cuda_errors(nvel->cu_device_compute_capability(&smmajor, &smminor, cu_device));

        smver = (smmajor << 4) | smminor;

        av_log(avctx, AV_LOG_VERBOSE, "[ GPU #%d - < %s > has Compute SM %d.%d, NVENC %s ]\n", i, gpu_name, smmajor, smminor, (smver >= target_smver) ? "Available" : "Not Available");

        if (smver >= target_smver)
            nvel->nvenc_devices[nvel->nvenc_device_count++] = cu_device;
    }

    if (!nvel->nvenc_device_count) {
        av_log(avctx, AV_LOG_FATAL, "No NVENC capable devices found\n");
        goto error;
    }

    return 1;

error:

    nvel->nvenc_device_count = 0;

    return 0;
}

static av_cold int nvenc_dyload_nvenc(AVCodecContext *avctx)
{
    PNVENCODEAPICREATEINSTANCE nvEncodeAPICreateInstance = 0;
    NVENCSTATUS nvstatus;

    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;

    if (!nvenc_check_cuda(avctx))
        return 0;

    if (nvel->nvenc_lib)
        return 1;

#if defined(_WIN32)
    if (sizeof(void*) == 8) {
        nvel->nvenc_lib = LoadLibrary(TEXT("nvEncodeAPI64.dll"));
    } else {
        nvel->nvenc_lib = LoadLibrary(TEXT("nvEncodeAPI.dll"));
    }
#else
    nvel->nvenc_lib = dlopen("libnvidia-encode.so.1", RTLD_LAZY);
#endif

    if (!nvel->nvenc_lib) {
        av_log(avctx, AV_LOG_FATAL, "Failed loading the nvenc library\n");
        goto error;
    }

    nvEncodeAPICreateInstance = (PNVENCODEAPICREATEINSTANCE)LOAD_FUNC(nvel->nvenc_lib, "NvEncodeAPICreateInstance");

    if (!nvEncodeAPICreateInstance) {
        av_log(avctx, AV_LOG_FATAL, "Failed to load nvenc entrypoint\n");
        goto error;
    }

    nvel->nvenc_funcs.version = NV_ENCODE_API_FUNCTION_LIST_VER;

    nvstatus = nvEncodeAPICreateInstance(&nvel->nvenc_funcs);

    if (nvstatus != NV_ENC_SUCCESS) {
        nvenc_print_error(avctx, nvstatus, "Failed to create nvenc instance");
        goto error;
    }

    av_log(avctx, AV_LOG_VERBOSE, "Nvenc initialized successfully\n");

    return 1;

error:
    if (nvel->nvenc_lib)
        DL_CLOSE_FUNC(nvel->nvenc_lib);

    nvel->nvenc_lib = NULL;

    return 0;
}

static av_cold void nvenc_unload_nvenc(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;

    DL_CLOSE_FUNC(nvel->nvenc_lib);
    nvel->nvenc_lib = NULL;

    nvel->nvenc_device_count = 0;

#if !CONFIG_CUDA
    DL_CLOSE_FUNC(nvel->cuda_lib);
    nvel->cuda_lib = NULL;
#endif

    nvel->cu_init = NULL;
    nvel->cu_device_get_count = NULL;
    nvel->cu_device_get = NULL;
    nvel->cu_device_get_name = NULL;
    nvel->cu_device_compute_capability = NULL;
    nvel->cu_ctx_create = NULL;
    nvel->cu_ctx_pop_current = NULL;
    nvel->cu_ctx_destroy = NULL;

    av_log(avctx, AV_LOG_VERBOSE, "Nvenc unloaded\n");
}

static av_cold int nvenc_setup_device(AVCodecContext *avctx)
{
    NVENCContext *ctx         = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;

    CUresult cu_res;
    CUcontext cu_context_curr;

    switch (avctx->codec->id) {
    case AV_CODEC_ID_H264:
        ctx->params.encodeGUID = NV_ENC_CODEC_H264_GUID;
        break;
    case AV_CODEC_ID_HEVC:
        ctx->params.encodeGUID = NV_ENC_CODEC_HEVC_GUID;
        break;
    default:
        return AVERROR_BUG;
    }

    ctx->data_pix_fmt = avctx->pix_fmt;

#if CONFIG_CUDA
    if (avctx->pix_fmt == AV_PIX_FMT_CUDA) {
        AVHWFramesContext   *frames_ctx;
        AVCUDADeviceContext *device_hwctx;

        if (!avctx->hw_frames_ctx) {
            av_log(avctx, AV_LOG_ERROR, "hw_frames_ctx must be set when using GPU frames as input\n");
            return AVERROR(EINVAL);
        }

        frames_ctx   = (AVHWFramesContext*)avctx->hw_frames_ctx->data;
        device_hwctx = frames_ctx->device_ctx->hwctx;

        ctx->cu_context = device_hwctx->cuda_ctx;
        ctx->data_pix_fmt = frames_ctx->sw_format;
        return 0;
    }
#endif

    if (ctx->gpu >= nvel->nvenc_device_count) {
        av_log(avctx, AV_LOG_FATAL, "Requested GPU %d, but only %d GPUs are available!\n", ctx->gpu, nvel->nvenc_device_count);
        return AVERROR(EINVAL);
    }

    ctx->cu_context = NULL;
    cu_res = nvel->cu_ctx_create(&ctx->cu_context_internal, 4, nvel->nvenc_devices[ctx->gpu]); // CU_CTX_SCHED_BLOCKING_SYNC=4, avoid CPU spins

    if (cu_res != CUDA_SUCCESS) {
        av_log(avctx, AV_LOG_FATAL, "Failed creating CUDA context for NVENC: 0x%x\n", (int)cu_res);
        return AVERROR_EXTERNAL;
    }

    cu_res = nvel->cu_ctx_pop_current(&cu_context_curr);

    if (cu_res != CUDA_SUCCESS) {
        av_log(avctx, AV_LOG_FATAL, "Failed popping CUDA context: 0x%x\n", (int)cu_res);
        return AVERROR_EXTERNAL;
    }

    ctx->cu_context = ctx->cu_context_internal;

    return 0;
}

typedef struct GUIDTuple {
    const GUID guid;
    int flags;
} GUIDTuple;

static int nvenc_map_preset(NVENCContext *ctx)
{
    GUIDTuple presets[] = {
        { NV_ENC_PRESET_DEFAULT_GUID },
        { NV_ENC_PRESET_HP_GUID },
        { NV_ENC_PRESET_HQ_GUID },
        { NV_ENC_PRESET_BD_GUID },
        { NV_ENC_PRESET_LOW_LATENCY_DEFAULT_GUID, NVENC_LOWLATENCY },
        { NV_ENC_PRESET_LOW_LATENCY_HP_GUID,      NVENC_LOWLATENCY },
        { NV_ENC_PRESET_LOW_LATENCY_HQ_GUID,      NVENC_LOWLATENCY },
        { NV_ENC_PRESET_LOSSLESS_DEFAULT_GUID,    NVENC_LOSSLESS },
        { NV_ENC_PRESET_LOSSLESS_HP_GUID,         NVENC_LOSSLESS },
        { { 0 } }
    };

    GUIDTuple *t = &presets[ctx->preset];

    ctx->params.presetGUID = t->guid;
    ctx->flags             = t->flags;

    return AVERROR(EINVAL);
}

static av_cold void set_constqp(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;

    ctx->config.rcParams.rateControlMode = NV_ENC_PARAMS_RC_CONSTQP;
    ctx->config.rcParams.constQP.qpInterB = avctx->global_quality;
    ctx->config.rcParams.constQP.qpInterP = avctx->global_quality;
    ctx->config.rcParams.constQP.qpIntra = avctx->global_quality;
}

static av_cold void set_vbr(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;

    ctx->config.rcParams.enableMinQP = 1;
    ctx->config.rcParams.enableMaxQP = 1;

    ctx->config.rcParams.minQP.qpInterB = avctx->qmin;
    ctx->config.rcParams.minQP.qpInterP = avctx->qmin;
    ctx->config.rcParams.minQP.qpIntra = avctx->qmin;

    ctx->config.rcParams.maxQP.qpInterB = avctx->qmax;
    ctx->config.rcParams.maxQP.qpInterP = avctx->qmax;
    ctx->config.rcParams.maxQP.qpIntra = avctx->qmax;
}

static av_cold void set_lossless(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;

    ctx->config.rcParams.rateControlMode = NV_ENC_PARAMS_RC_CONSTQP;
    ctx->config.rcParams.constQP.qpInterB = 0;
    ctx->config.rcParams.constQP.qpInterP = 0;
    ctx->config.rcParams.constQP.qpIntra = 0;
}

static av_cold void nvenc_setup_rate_control(AVCodecContext *avctx)
{
    NVENCContext *ctx    = avctx->priv_data;

    int qp_inter_p;

    if (avctx->bit_rate > 0) {
        ctx->config.rcParams.averageBitRate = avctx->bit_rate;
    } else if (ctx->config.rcParams.averageBitRate > 0) {
        ctx->config.rcParams.maxBitRate = ctx->config.rcParams.averageBitRate;
    }

    if (avctx->rc_max_rate > 0)
        ctx->config.rcParams.maxBitRate = avctx->rc_max_rate;

    if (ctx->flags & NVENC_LOSSLESS) {
        set_lossless(avctx);

        avctx->qmin = -1;
        avctx->qmax = -1;
    } else if (ctx->cbr) {
        if (!ctx->twopass) {
            ctx->config.rcParams.rateControlMode = NV_ENC_PARAMS_RC_CBR;
        } else {
            ctx->config.rcParams.rateControlMode = NV_ENC_PARAMS_RC_2_PASS_QUALITY;

            if (avctx->codec->id == AV_CODEC_ID_H264) {
                ctx->config.encodeCodecConfig.h264Config.adaptiveTransformMode = NV_ENC_H264_ADAPTIVE_TRANSFORM_ENABLE;
                ctx->config.encodeCodecConfig.h264Config.fmoMode = NV_ENC_H264_FMO_DISABLE;
            }
        }

        if (avctx->codec->id == AV_CODEC_ID_H264) {
            ctx->config.encodeCodecConfig.h264Config.outputBufferingPeriodSEI = 1;
            ctx->config.encodeCodecConfig.h264Config.outputPictureTimingSEI = 1;
        } else if(avctx->codec->id == AV_CODEC_ID_H265) {
            ctx->config.encodeCodecConfig.hevcConfig.outputBufferingPeriodSEI = 1;
            ctx->config.encodeCodecConfig.hevcConfig.outputPictureTimingSEI = 1;
        }
    } else if (avctx->global_quality > 0) {
        set_constqp(avctx);

        avctx->qmin = -1;
        avctx->qmax = -1;
    } else {
        if (avctx->qmin >= 0 && avctx->qmax >= 0) {
            set_vbr(avctx);

            qp_inter_p = (avctx->qmax + 3 * avctx->qmin) / 4; // biased towards Qmin

            if (ctx->twopass) {
                ctx->config.rcParams.rateControlMode = NV_ENC_PARAMS_RC_2_PASS_VBR;
                if (avctx->codec->id == AV_CODEC_ID_H264) {
                    ctx->config.encodeCodecConfig.h264Config.adaptiveTransformMode = NV_ENC_H264_ADAPTIVE_TRANSFORM_ENABLE;
                    ctx->config.encodeCodecConfig.h264Config.fmoMode = NV_ENC_H264_FMO_DISABLE;
                }
            } else {
                ctx->config.rcParams.rateControlMode = NV_ENC_PARAMS_RC_VBR_MINQP;
            }
        } else {
            qp_inter_p = 26; // default to 26

            if (ctx->twopass) {
                ctx->config.rcParams.rateControlMode = NV_ENC_PARAMS_RC_2_PASS_VBR;
            } else {
                ctx->config.rcParams.rateControlMode = NV_ENC_PARAMS_RC_VBR;
            }
        }

        ctx->config.rcParams.enableInitialRCQP = 1;
        ctx->config.rcParams.initialRCQP.qpInterP  = qp_inter_p;

        if(avctx->i_quant_factor != 0.0 && avctx->b_quant_factor != 0.0) {
            ctx->config.rcParams.initialRCQP.qpIntra = av_clip(
                qp_inter_p * fabs(avctx->i_quant_factor) + avctx->i_quant_offset, 0, 51);
            ctx->config.rcParams.initialRCQP.qpInterB = av_clip(
                qp_inter_p * fabs(avctx->b_quant_factor) + avctx->b_quant_offset, 0, 51);
        } else {
            ctx->config.rcParams.initialRCQP.qpIntra = qp_inter_p;
            ctx->config.rcParams.initialRCQP.qpInterB = qp_inter_p;
        }
    }

    if (avctx->rc_buffer_size > 0) {
        ctx->config.rcParams.vbvBufferSize = avctx->rc_buffer_size;
    } else if (ctx->config.rcParams.averageBitRate > 0) {
        ctx->config.rcParams.vbvBufferSize = 2 * ctx->config.rcParams.averageBitRate;
    }
}

static av_cold int nvenc_setup_h264_config(AVCodecContext *avctx)
{
    NVENCContext *ctx                      = avctx->priv_data;
    NV_ENC_CONFIG *cc                      = &ctx->config;
    NV_ENC_CONFIG_H264 *h264               = &cc->encodeCodecConfig.h264Config;
    NV_ENC_CONFIG_H264_VUI_PARAMETERS *vui = &h264->h264VUIParameters;

    vui->colourDescriptionPresentFlag = avctx->colorspace      != AVCOL_SPC_UNSPECIFIED ||
                                        avctx->color_primaries != AVCOL_PRI_UNSPECIFIED ||
                                        avctx->color_trc       != AVCOL_TRC_UNSPECIFIED;

    vui->colourMatrix            = avctx->colorspace;
    vui->colourPrimaries         = avctx->color_primaries;
    vui->transferCharacteristics = avctx->color_trc;

    vui->videoFullRangeFlag = (avctx->color_range == AVCOL_RANGE_JPEG
        || ctx->data_pix_fmt == AV_PIX_FMT_YUVJ420P || ctx->data_pix_fmt == AV_PIX_FMT_YUVJ422P || ctx->data_pix_fmt == AV_PIX_FMT_YUVJ444P);

    vui->videoSignalTypePresentFlag =
        (vui->colourDescriptionPresentFlag
        || vui->videoFormat != 5
        || vui->videoFullRangeFlag != 0);

    h264->disableSPSPPS = (avctx->flags & AV_CODEC_FLAG_GLOBAL_HEADER) ? 1 : 0;
    h264->repeatSPSPPS  = (avctx->flags & AV_CODEC_FLAG_GLOBAL_HEADER) ? 0 : 1;
    h264->outputAUD     = 1;

    h264->sliceMode = 3;
    h264->sliceModeData = 1;

    if (ctx->flags & NVENC_LOSSLESS)
        h264->qpPrimeYZeroTransformBypassFlag = 1;

    switch(ctx->profile) {
    case NV_ENC_H264_PROFILE_BASELINE:
        cc->profileGUID = NV_ENC_H264_PROFILE_BASELINE_GUID;
        avctx->profile = FF_PROFILE_H264_BASELINE;
        break;
    case NV_ENC_H264_PROFILE_MAIN:
        cc->profileGUID = NV_ENC_H264_PROFILE_MAIN_GUID;
        avctx->profile = FF_PROFILE_H264_MAIN;
        break;
    case NV_ENC_H264_PROFILE_HIGH:
        cc->profileGUID = NV_ENC_H264_PROFILE_HIGH_GUID;
        avctx->profile = FF_PROFILE_H264_HIGH;
        break;
    case NV_ENC_H264_PROFILE_HIGH_444_P:
        cc->profileGUID = NV_ENC_H264_PROFILE_HIGH_444_GUID;
        avctx->profile = FF_PROFILE_H264_HIGH_444_PREDICTIVE;
        break;
    case NV_ENC_H264_PROFILE_CONSTRAINED_HIGH:
        cc->profileGUID = NV_ENC_H264_PROFILE_CONSTRAINED_HIGH_GUID;
        break;
    }

    // force setting profile as high444p if input is AV_PIX_FMT_YUV444P
    if (ctx->data_pix_fmt == AV_PIX_FMT_YUV444P) {
        cc->profileGUID = NV_ENC_H264_PROFILE_HIGH_444_GUID;
        avctx->profile = FF_PROFILE_H264_HIGH_444_PREDICTIVE;
    }

    h264->chromaFormatIDC = avctx->profile == FF_PROFILE_H264_HIGH_444_PREDICTIVE ? 3 : 1;

    h264->level = ctx->level;

    return 0;
}

static av_cold int nvenc_setup_hevc_config(AVCodecContext *avctx)
{
    NVENCContext *ctx                      = avctx->priv_data;
    NV_ENC_CONFIG *cc                      = &ctx->config;
    NV_ENC_CONFIG_HEVC *hevc               = &cc->encodeCodecConfig.hevcConfig;
    NV_ENC_CONFIG_HEVC_VUI_PARAMETERS *vui = &hevc->hevcVUIParameters;

    vui->colourDescriptionPresentFlag = avctx->colorspace      != AVCOL_SPC_UNSPECIFIED ||
                                        avctx->color_primaries != AVCOL_PRI_UNSPECIFIED ||
                                        avctx->color_trc       != AVCOL_TRC_UNSPECIFIED;

    vui->colourMatrix            = avctx->colorspace;
    vui->colourPrimaries         = avctx->color_primaries;
    vui->transferCharacteristics = avctx->color_trc;

    vui->videoFullRangeFlag = (avctx->color_range == AVCOL_RANGE_JPEG
        || ctx->data_pix_fmt == AV_PIX_FMT_YUVJ420P || ctx->data_pix_fmt == AV_PIX_FMT_YUVJ422P || ctx->data_pix_fmt == AV_PIX_FMT_YUVJ444P);

    vui->videoSignalTypePresentFlag =
        (vui->colourDescriptionPresentFlag
        || vui->videoFormat != 5
        || vui->videoFullRangeFlag != 0);

    hevc->disableSPSPPS = (avctx->flags & AV_CODEC_FLAG_GLOBAL_HEADER) ? 1 : 0;
    hevc->repeatSPSPPS  = (avctx->flags & AV_CODEC_FLAG_GLOBAL_HEADER) ? 0 : 1;
    hevc->outputAUD     = 1;

    /* No other profile is supported in the current SDK version 5 */
    cc->profileGUID = NV_ENC_HEVC_PROFILE_MAIN_GUID;
    avctx->profile  = FF_PROFILE_HEVC_MAIN;

    hevc->sliceMode = 3;
    hevc->sliceModeData = 1;

    hevc->level = ctx->level;

    hevc->tier = ctx->tier;

    return 0;
}

static av_cold int nvenc_setup_codec_config(AVCodecContext *avctx)
{
    switch (avctx->codec->id) {
    case AV_CODEC_ID_H264:
        return nvenc_setup_h264_config(avctx);
    case AV_CODEC_ID_HEVC:
        return nvenc_setup_hevc_config(avctx);
    /* Earlier switch/case will return if unknown codec is passed. */
    }

    return 0;
}

static av_cold int nvenc_setup_encoder(AVCodecContext *avctx)
{
    NVENCContext *ctx               = avctx->priv_data;
    NV_ENCODE_API_FUNCTION_LIST *nv = &ctx->nvel.nvenc_funcs;
    NV_ENC_PRESET_CONFIG preset_cfg = { 0 };
    NVENCSTATUS ret;
    AVCPBProperties *cpb_props;
    int num_mbs;
    int res = 0;

    ctx->last_dts = AV_NOPTS_VALUE;

    ctx->config.version = NV_ENC_CONFIG_VER;
    ctx->params.version = NV_ENC_INITIALIZE_PARAMS_VER;

    ctx->params.encodeHeight = avctx->height;
    ctx->params.encodeWidth  = avctx->width;

    if (avctx->sample_aspect_ratio.num &&
        avctx->sample_aspect_ratio.den &&
        (avctx->sample_aspect_ratio.num != 1 ||
         avctx->sample_aspect_ratio.den != 1)) {
        av_reduce(&ctx->params.darWidth,
                  &ctx->params.darHeight,
                  avctx->width * avctx->sample_aspect_ratio.num,
                  avctx->height * avctx->sample_aspect_ratio.den,
                  1024 * 1024);
    } else {
        ctx->params.darHeight = avctx->height;
        ctx->params.darWidth  = avctx->width;
    }

    // De-compensate for hardware, dubiously, trying to compensate for
    // playback at 704 pixel width.
    if (avctx->width == 720 && (avctx->height == 480 || avctx->height == 576)) {
        av_reduce(&ctx->params.darWidth, &ctx->params.darHeight,
                  ctx->params.darWidth * 44,
                  ctx->params.darHeight * 45,
                  1024 * 1024);
    }

    ctx->params.frameRateNum = avctx->time_base.den;
    ctx->params.frameRateDen = avctx->time_base.num * avctx->ticks_per_frame;

    num_mbs = ((avctx->width + 15) >> 4) * ((avctx->height + 15) >> 4);
    ctx->nb_surfaces = (num_mbs >= 8160) ? 32 : 48;

    if (ctx->buffer_delay >= ctx->nb_surfaces)
        ctx->buffer_delay = ctx->nb_surfaces - 1;

    ctx->params.enableEncodeAsync = 0;
    ctx->params.enablePTD         = 1;

    ctx->params.encodeConfig = &ctx->config;

    nvenc_map_preset(ctx);

    preset_cfg.version           = NV_ENC_PRESET_CONFIG_VER;
    preset_cfg.presetCfg.version = NV_ENC_CONFIG_VER;

    if (ctx->twopass < 0) {
        ctx->twopass = (ctx->flags & NVENC_LOWLATENCY) ? 1 : 0;
    }

    ret = nv->nvEncGetEncodePresetConfig(ctx->nvenc_ctx,
                                         ctx->params.encodeGUID,
                                         ctx->params.presetGUID,
                                         &preset_cfg);
    if (ret != NV_ENC_SUCCESS)
        return nvenc_print_error(avctx, ret, "Cannot get the preset configuration");

    memcpy(&ctx->config, &preset_cfg.presetCfg, sizeof(ctx->config));

    ctx->config.version = NV_ENC_CONFIG_VER;

    if (avctx->refs >= 0) {
        /* 0 means "let the hardware decide" */
        switch (avctx->codec->id) {
        case AV_CODEC_ID_H264:
            ctx->config.encodeCodecConfig.h264Config.maxNumRefFrames = avctx->refs;
            break;
        case AV_CODEC_ID_H265:
            ctx->config.encodeCodecConfig.hevcConfig.maxNumRefFramesInDPB = avctx->refs;
            break;
        /* Earlier switch/case will return if unknown codec is passed. */
        }
    }

    if (avctx->gop_size > 0) {
        if (avctx->max_b_frames >= 0) {
            /* 0 is intra-only, 1 is I/P only, 2 is one B Frame, 3 two B frames, and so on. */
            ctx->config.frameIntervalP = avctx->max_b_frames + 1;
        }

        ctx->config.gopLength = avctx->gop_size;
        switch (avctx->codec->id) {
        case AV_CODEC_ID_H264:
            ctx->config.encodeCodecConfig.h264Config.idrPeriod = avctx->gop_size;
            break;
        case AV_CODEC_ID_H265:
            ctx->config.encodeCodecConfig.hevcConfig.idrPeriod = avctx->gop_size;
            break;
        /* Earlier switch/case will return if unknown codec is passed. */
        }
    } else if (avctx->gop_size == 0) {
        ctx->config.frameIntervalP = 0;
        ctx->config.gopLength      = 1;
        switch (avctx->codec->id) {
        case AV_CODEC_ID_H264:
            ctx->config.encodeCodecConfig.h264Config.idrPeriod = 1;
            break;
        case AV_CODEC_ID_H265:
            ctx->config.encodeCodecConfig.hevcConfig.idrPeriod = 1;
            break;
        /* Earlier switch/case will return if unknown codec is passed. */
        }
    }

    /* when there're b frames, set dts offset */
    if (ctx->config.frameIntervalP >= 2)
        ctx->last_dts = -2;

    nvenc_setup_rate_control(avctx);

    if (avctx->flags & AV_CODEC_FLAG_INTERLACED_DCT) {
        ctx->config.frameFieldMode = NV_ENC_PARAMS_FRAME_FIELD_MODE_FIELD;
    } else {
        ctx->config.frameFieldMode = NV_ENC_PARAMS_FRAME_FIELD_MODE_FRAME;
    }

    res = nvenc_setup_codec_config(avctx);
    if (res)
        return res;

    ret = nv->nvEncInitializeEncoder(ctx->nvenc_ctx, &ctx->params);
    if (ret != NV_ENC_SUCCESS)
        return nvenc_print_error(avctx, ret, "Cannot initialize the decoder");

    if (ctx->config.frameIntervalP > 1)
        avctx->has_b_frames = 2;

    if (ctx->config.rcParams.averageBitRate > 0)
        avctx->bit_rate = ctx->config.rcParams.averageBitRate;

    cpb_props = ff_add_cpb_side_data(avctx);
    if (!cpb_props)
        return AVERROR(ENOMEM);
    cpb_props->max_bitrate = ctx->config.rcParams.maxBitRate;
    cpb_props->avg_bitrate = avctx->bit_rate;
    cpb_props->buffer_size = ctx->config.rcParams.vbvBufferSize;

    return 0;
}

static av_cold int nvenc_alloc_surface(AVCodecContext *avctx, int idx)
{
    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;
    NV_ENCODE_API_FUNCTION_LIST *nv = &nvel->nvenc_funcs;

    NVENCSTATUS nv_status;
    NV_ENC_CREATE_BITSTREAM_BUFFER allocOut = { 0 };
    allocOut.version = NV_ENC_CREATE_BITSTREAM_BUFFER_VER;

    switch (ctx->data_pix_fmt) {
    case AV_PIX_FMT_YUV420P:
        ctx->frames[idx].format = NV_ENC_BUFFER_FORMAT_YV12_PL;
        break;
    case AV_PIX_FMT_NV12:
        ctx->frames[idx].format = NV_ENC_BUFFER_FORMAT_NV12_PL;
        break;
    case AV_PIX_FMT_YUV444P:
        ctx->frames[idx].format = NV_ENC_BUFFER_FORMAT_YUV444_PL;
        break;
    default:
        av_log(avctx, AV_LOG_FATAL, "Invalid input pixel format\n");
        return AVERROR(EINVAL);
    }

    if (avctx->pix_fmt == AV_PIX_FMT_CUDA) {
        ctx->frames[idx].in_ref = av_frame_alloc();
        if (!ctx->frames[idx].in_ref)
            return AVERROR(ENOMEM);
    } else {
        NV_ENC_CREATE_INPUT_BUFFER allocSurf = { 0 };
        allocSurf.version = NV_ENC_CREATE_INPUT_BUFFER_VER;
        allocSurf.width = (avctx->width + 31) & ~31;
        allocSurf.height = (avctx->height + 31) & ~31;
        allocSurf.memoryHeap = NV_ENC_MEMORY_HEAP_SYSMEM_CACHED;
        allocSurf.bufferFmt = ctx->frames[idx].format;

        nv_status = nv->nvEncCreateInputBuffer(ctx->nvenc_ctx, &allocSurf);
        if (nv_status != NV_ENC_SUCCESS) {
            return nvenc_print_error(avctx, nv_status, "CreateInputBuffer failed");
        }

        ctx->frames[idx].in = allocSurf.inputBuffer;
        ctx->frames[idx].width = allocSurf.width;
        ctx->frames[idx].height = allocSurf.height;
    }

    ctx->frames[idx].locked = 0;

    /* 1MB is large enough to hold most output frames. NVENC increases this automaticaly if it's not enough. */
    allocOut.size = 1024 * 1024;

    allocOut.memoryHeap = NV_ENC_MEMORY_HEAP_SYSMEM_CACHED;

    nv_status = nv->nvEncCreateBitstreamBuffer(ctx->nvenc_ctx, &allocOut);
    if (nv_status != NV_ENC_SUCCESS) {
        int err = nvenc_print_error(avctx, nv_status, "CreateBitstreamBuffer failed");
        if (avctx->pix_fmt != AV_PIX_FMT_CUDA)
            nv->nvEncDestroyInputBuffer(ctx->nvenc_ctx, ctx->frames[idx].in);
        av_frame_free(&ctx->frames[idx].in_ref);
        return err;
    }

    ctx->frames[idx].out = allocOut.bitstreamBuffer;
    ctx->frames[idx].size = allocOut.size;

    return 0;
}

static av_cold int nvenc_setup_frames(AVCodecContext *avctx, int* surfaceCount)
{
    int res;
    NVENCContext *ctx = avctx->priv_data;

    ctx->frames = av_malloc(ctx->nb_surfaces * sizeof(*ctx->frames));

    if (!ctx->frames) {
        return AVERROR(ENOMEM);
    }

    ctx->timestamps = av_fifo_alloc(ctx->nb_surfaces * sizeof(int64_t));
    if (!ctx->timestamps)
        return AVERROR(ENOMEM);
    ctx->pending = av_fifo_alloc(ctx->nb_surfaces * sizeof(NVENCFrame*));
    if (!ctx->pending)
        return AVERROR(ENOMEM);
    ctx->ready = av_fifo_alloc(ctx->nb_surfaces * sizeof(NVENCFrame*));
    if (!ctx->ready)
        return AVERROR(ENOMEM);

    for(*surfaceCount = 0; *surfaceCount < ctx->nb_surfaces; ++*surfaceCount) {
        res = nvenc_alloc_surface(avctx, *surfaceCount);
        if (res)
            return res;
    }

    return 0;
}

static av_cold int nvenc_setup_extradata(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;
    NV_ENCODE_API_FUNCTION_LIST *nv = &nvel->nvenc_funcs;

    NVENCSTATUS nv_status;
    uint32_t outSize = 0;
    char tmpHeader[256];
    NV_ENC_SEQUENCE_PARAM_PAYLOAD payload = { 0 };
    payload.version = NV_ENC_SEQUENCE_PARAM_PAYLOAD_VER;

    payload.spsppsBuffer = tmpHeader;
    payload.inBufferSize = sizeof(tmpHeader);
    payload.outSPSPPSPayloadSize = &outSize;

    nv_status = nv->nvEncGetSequenceParams(ctx->nvenc_ctx, &payload);
    if (nv_status != NV_ENC_SUCCESS) {
        return nvenc_print_error(avctx, nv_status, "GetSequenceParams failed");
    }

    avctx->extradata_size = outSize;
    avctx->extradata = av_mallocz(outSize + AV_INPUT_BUFFER_PADDING_SIZE);

    if (!avctx->extradata) {
        return AVERROR(ENOMEM);
    }

    memcpy(avctx->extradata, tmpHeader, outSize);

    return 0;
}

av_cold int ff_nvenc_encode_init(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;
    NV_ENCODE_API_FUNCTION_LIST *nv = &nvel->nvenc_funcs;

    int res;
    int i;
    int surfaceCount = 0;

    if (!nvenc_dyload_nvenc(avctx))
        return AVERROR_EXTERNAL;

    res = nvenc_setup_device(avctx);
    if (res)
        goto error;

    res = nvenc_open_session(avctx);
    if (res)
        goto error;

    res = nvenc_setup_encoder(avctx);
    if (res)
        goto error;

    res = nvenc_setup_frames(avctx, &surfaceCount);
    if (res)
        goto error;

    if (avctx->flags & AV_CODEC_FLAG_GLOBAL_HEADER) {
        res = nvenc_setup_extradata(avctx);
        if (res)
            goto error;
    }

    return 0;

error:
    av_fifo_freep(&ctx->timestamps);
    av_fifo_freep(&ctx->ready);
    av_fifo_freep(&ctx->pending);

    for (i = 0; i < surfaceCount; ++i) {
        if (avctx->pix_fmt != AV_PIX_FMT_CUDA)
            nv->nvEncDestroyInputBuffer(ctx->nvenc_ctx, ctx->frames[i].in);
        av_frame_free(&ctx->frames[i].in_ref);
        nv->nvEncDestroyBitstreamBuffer(ctx->nvenc_ctx, ctx->frames[i].out);
    }
    av_freep(&ctx->frames);

    if (ctx->nvenc_ctx)
        nv->nvEncDestroyEncoder(ctx->nvenc_ctx);
    ctx->nvenc_ctx = NULL;

    if (ctx->cu_context_internal)
        nvel->cu_ctx_destroy(ctx->cu_context_internal);
    ctx->cu_context = ctx->cu_context_internal = NULL;

    nvenc_unload_nvenc(avctx);

    return res;
}

av_cold int ff_nvenc_encode_close(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;
    NV_ENCODE_API_FUNCTION_LIST *nv = &nvel->nvenc_funcs;
    int i;

    av_fifo_freep(&ctx->timestamps);
    av_fifo_freep(&ctx->ready);
    av_fifo_freep(&ctx->pending);

    if (avctx->pix_fmt == AV_PIX_FMT_CUDA) {
        for (i = 0; i < ctx->nb_surfaces; ++i) {
            if (ctx->frames[i].in) {
                 nv->nvEncUnmapInputResource(ctx->nvenc_ctx, ctx->frames[i].in_map.mappedResource);
            }
        }
        for (i = 0; i < ctx->nb_registered_frames; i++) {
            if (ctx->registered_frames[i].regptr)
                nv->nvEncUnregisterResource(ctx->nvenc_ctx, ctx->registered_frames[i].regptr);
        }
        ctx->nb_registered_frames = 0;
    }

    for (i = 0; i < ctx->nb_surfaces; ++i) {
        if (avctx->pix_fmt != AV_PIX_FMT_CUDA)
            nv->nvEncDestroyInputBuffer(ctx->nvenc_ctx, ctx->frames[i].in);
        av_frame_free(&ctx->frames[i].in_ref);
        nv->nvEncDestroyBitstreamBuffer(ctx->nvenc_ctx, ctx->frames[i].out);
    }
    av_freep(&ctx->frames);
    ctx->nb_surfaces = 0;

    nv->nvEncDestroyEncoder(ctx->nvenc_ctx);
    ctx->nvenc_ctx = NULL;

    if (ctx->cu_context_internal)
        nvel->cu_ctx_destroy(ctx->cu_context_internal);
    ctx->cu_context = ctx->cu_context_internal = NULL;

    nvenc_unload_nvenc(avctx);

    return 0;
}

static NVENCFrame *get_free_frame(NVENCContext *ctx)
{
    int i;

    for (i = 0; i < ctx->nb_surfaces; ++i) {
        if (!ctx->frames[i].locked) {
            ctx->frames[i].locked = 1;
            return &ctx->frames[i];
        }
    }

    return NULL;
}

static int nvenc_copy_frame(AVCodecContext *avctx, NVENCFrame *inSurf, 
            NV_ENC_LOCK_INPUT_BUFFER *lockBufferParams, const AVFrame *frame)
{
    uint8_t *buf = lockBufferParams->bufferDataPtr;
    int off = inSurf->height * lockBufferParams->pitch;

    if (frame->format == AV_PIX_FMT_YUV420P) {
        av_image_copy_plane(buf, lockBufferParams->pitch,
            frame->data[0], frame->linesize[0],
            avctx->width, avctx->height);

        buf += off;

        av_image_copy_plane(buf, lockBufferParams->pitch >> 1,
            frame->data[2], frame->linesize[2],
            avctx->width >> 1, avctx->height >> 1);

        buf += off >> 2;

        av_image_copy_plane(buf, lockBufferParams->pitch >> 1,
            frame->data[1], frame->linesize[1],
            avctx->width >> 1, avctx->height >> 1);
    } else if (frame->format == AV_PIX_FMT_NV12) {
        av_image_copy_plane(buf, lockBufferParams->pitch,
            frame->data[0], frame->linesize[0],
            avctx->width, avctx->height);

        buf += off;

        av_image_copy_plane(buf, lockBufferParams->pitch,
            frame->data[1], frame->linesize[1],
            avctx->width, avctx->height >> 1);
    } else if (frame->format == AV_PIX_FMT_YUV444P) {
        av_image_copy_plane(buf, lockBufferParams->pitch,
            frame->data[0], frame->linesize[0],
            avctx->width, avctx->height);

        buf += off;

        av_image_copy_plane(buf, lockBufferParams->pitch,
            frame->data[1], frame->linesize[1],
            avctx->width, avctx->height);

        buf += off;

        av_image_copy_plane(buf, lockBufferParams->pitch,
            frame->data[2], frame->linesize[2],
            avctx->width, avctx->height);
    } else {
        av_log(avctx, AV_LOG_FATAL, "Invalid pixel format!\n");
        return AVERROR(EINVAL);
    }

    return 0;
}

static int nvenc_find_free_reg_resource(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;
    NV_ENCODE_API_FUNCTION_LIST *nv = &nvel->nvenc_funcs;

    int i;

    if (ctx->nb_registered_frames == FF_ARRAY_ELEMS(ctx->registered_frames)) {
        for (i = 0; i < ctx->nb_registered_frames; i++) {
            if (!ctx->registered_frames[i].mapped) {
                if (ctx->registered_frames[i].regptr) {
                    nv->nvEncUnregisterResource(ctx->nvenc_ctx,
                                                ctx->registered_frames[i].regptr);
                    ctx->registered_frames[i].regptr = NULL;
                }
                return i;
            }
        }
    } else {
        return ctx->nb_registered_frames++;
    }

    av_log(avctx, AV_LOG_ERROR, "Too many registered CUDA frames\n");
    return AVERROR(ENOMEM);
}

static int nvenc_register_frame(AVCodecContext *avctx, const AVFrame *frame)
{
    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;
    NV_ENCODE_API_FUNCTION_LIST *nv = &nvel->nvenc_funcs;

    AVHWFramesContext *frames_ctx = (AVHWFramesContext*)avctx->hw_frames_ctx->data;
    NV_ENC_REGISTER_RESOURCE reg;
    int i, idx, ret;

    for (i = 0; i < ctx->nb_registered_frames; i++) {
        if (ctx->registered_frames[i].ptr == (CUdeviceptr)frame->data[0])
            return i;
    }

    idx = nvenc_find_free_reg_resource(avctx);
    if (idx < 0)
        return idx;

    reg.version            = NV_ENC_REGISTER_RESOURCE_VER;
    reg.resourceType       = NV_ENC_INPUT_RESOURCE_TYPE_CUDADEVICEPTR;
    reg.width              = frames_ctx->width;
    reg.height             = frames_ctx->height;
    reg.bufferFormat       = ctx->frames[0].format;
    reg.pitch              = frame->linesize[0];
    reg.resourceToRegister = frame->data[0];

    ret = nv->nvEncRegisterResource(ctx->nvenc_ctx, &reg);
    if (ret != NV_ENC_SUCCESS) {
        nvenc_print_error(avctx, ret, "Error registering an input resource");
        return AVERROR_UNKNOWN;
    }

    ctx->registered_frames[idx].ptr    = (CUdeviceptr)frame->data[0];
    ctx->registered_frames[idx].regptr = reg.registeredResource;
    return idx;
}

static int nvenc_upload_frame(AVCodecContext *avctx, const AVFrame *frame,
                                      NVENCFrame *nvenc_frame)
{
    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;
    NV_ENCODE_API_FUNCTION_LIST *nv = &nvel->nvenc_funcs;

    int res;
    NVENCSTATUS nv_status;

    if (avctx->pix_fmt == AV_PIX_FMT_CUDA) {
        int reg_idx = nvenc_register_frame(avctx, frame);
        if (reg_idx < 0) {
            av_log(avctx, AV_LOG_ERROR, "Could not register an input CUDA frame\n");
            return reg_idx;
        }
        
        res = av_frame_ref(nvenc_frame->in_ref, frame);
        if (res < 0)
            return res;
        
        nvenc_frame->in_map.version = NV_ENC_MAP_INPUT_RESOURCE_VER;
        nvenc_frame->in_map.registeredResource = ctx->registered_frames[reg_idx].regptr;
        nv_status = nv->nvEncMapInputResource(ctx->nvenc_ctx, &nvenc_frame->in_map);
        if (nv_status != NV_ENC_SUCCESS) {
            av_frame_unref(nvenc_frame->in_ref);
            return nvenc_print_error(avctx, nv_status, "Error mapping an input resource");
        }

        ctx->registered_frames[reg_idx].mapped = 1;
        nvenc_frame->reg_idx                   = reg_idx;
        nvenc_frame->in             = nvenc_frame->in_map.mappedResource;
        return 0;
    } else {
        NV_ENC_LOCK_INPUT_BUFFER lockBufferParams = { 0 };

        lockBufferParams.version = NV_ENC_LOCK_INPUT_BUFFER_VER;
        lockBufferParams.inputBuffer = nvenc_frame->in;

        nv_status = nv->nvEncLockInputBuffer(ctx->nvenc_ctx, &lockBufferParams);
        if (nv_status != NV_ENC_SUCCESS) {
            return nvenc_print_error(avctx, nv_status, "Failed locking nvenc input buffer");
        }

        res = nvenc_copy_frame(avctx, nvenc_frame, &lockBufferParams, frame);

        nv_status = nv->nvEncUnlockInputBuffer(ctx->nvenc_ctx, nvenc_frame->in);
        if (nv_status != NV_ENC_SUCCESS) {
            return nvenc_print_error(avctx, nv_status, "Failed unlocking input buffer!");
        }

        return res;
    }
}

static void nvenc_codec_specific_pic_params(AVCodecContext *avctx,
                                            NV_ENC_PIC_PARAMS *params)
{
    NVENCContext *ctx = avctx->priv_data;

    switch (avctx->codec->id) {
    case AV_CODEC_ID_H264:
        params->codecPicParams.h264PicParams.sliceMode =
            ctx->config.encodeCodecConfig.h264Config.sliceMode;
        params->codecPicParams.h264PicParams.sliceModeData =
            ctx->config.encodeCodecConfig.h264Config.sliceModeData;
        break;
    case AV_CODEC_ID_H265:
        params->codecPicParams.hevcPicParams.sliceMode =
            ctx->config.encodeCodecConfig.hevcConfig.sliceMode;
        params->codecPicParams.hevcPicParams.sliceModeData =
            ctx->config.encodeCodecConfig.hevcConfig.sliceModeData;
      break;
    }
}

static void nvenc_enqueue_timestamp(AVFifoBuffer* f, int64_t pts)
{
    av_fifo_generic_write(f, &pts, sizeof(pts), NULL);
}

static int64_t nvenc_dequeue_timestamp(AVFifoBuffer* queue)
{
    int64_t timestamp = AV_NOPTS_VALUE;
    if (av_fifo_size(queue) > 0)
        av_fifo_generic_read(queue, &timestamp, sizeof(timestamp), NULL);

    return timestamp;
}

static int process_out(AVCodecContext *avctx, AVPacket *pkt, NVENCFrame *tmpoutsurf)
{
    NVENCContext *ctx = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;
    NV_ENCODE_API_FUNCTION_LIST *nv = &nvel->nvenc_funcs;

    uint32_t slice_mode_data;
    uint32_t *slice_offsets;
    NV_ENC_LOCK_BITSTREAM lock_params = { 0 };
    NVENCSTATUS nv_status;
    int res = 0;

    enum AVPictureType pict_type;

    switch (avctx->codec->id) {
    case AV_CODEC_ID_H264:
      slice_mode_data = ctx->config.encodeCodecConfig.h264Config.sliceModeData;
      break;
    case AV_CODEC_ID_H265:
      slice_mode_data = ctx->config.encodeCodecConfig.hevcConfig.sliceModeData;
      break;
    default:
      av_log(avctx, AV_LOG_ERROR, "Unknown codec name\n");
      res = AVERROR(EINVAL);
      goto error;
    }
    slice_offsets = av_mallocz(slice_mode_data * sizeof(*slice_offsets));

    if (!slice_offsets)
        return AVERROR(ENOMEM);

    lock_params.version = NV_ENC_LOCK_BITSTREAM_VER;

    lock_params.doNotWait = 0;
    lock_params.outputBitstream = tmpoutsurf->out;
    lock_params.sliceOffsets = slice_offsets;

    nv_status = nv->nvEncLockBitstream(ctx->nvenc_ctx, &lock_params);
    if (nv_status != NV_ENC_SUCCESS) {
        res = nvenc_print_error(avctx, nv_status, "Failed locking bitstream buffer");
        goto error;
    }

    if (res = ff_alloc_packet2(avctx, pkt, lock_params.bitstreamSizeInBytes,0)) {
        nv->nvEncUnlockBitstream(ctx->nvenc_ctx, tmpoutsurf->out);
        goto error;
    }

    memcpy(pkt->data, lock_params.bitstreamBufferPtr, lock_params.bitstreamSizeInBytes);

    nv_status = nv->nvEncUnlockBitstream(ctx->nvenc_ctx, tmpoutsurf->out);
    if (nv_status != NV_ENC_SUCCESS)
        nvenc_print_error(avctx, nv_status, "Failed unlocking bitstream buffer, expect the gates of mordor to open");


    if (avctx->pix_fmt == AV_PIX_FMT_CUDA) {
        nv->nvEncUnmapInputResource(ctx->nvenc_ctx, tmpoutsurf->in_map.mappedResource);
        av_frame_unref(tmpoutsurf->in_ref);
        ctx->registered_frames[tmpoutsurf->reg_idx].mapped = 0;

        tmpoutsurf->in = NULL;
    }

    switch (lock_params.pictureType) {
    case NV_ENC_PIC_TYPE_IDR:
        pkt->flags |= AV_PKT_FLAG_KEY;
    case NV_ENC_PIC_TYPE_I:
        pict_type = AV_PICTURE_TYPE_I;
        break;
    case NV_ENC_PIC_TYPE_P:
        pict_type = AV_PICTURE_TYPE_P;
        break;
    case NV_ENC_PIC_TYPE_B:
        pict_type = AV_PICTURE_TYPE_B;
        break;
    case NV_ENC_PIC_TYPE_BI:
        pict_type = AV_PICTURE_TYPE_BI;
        break;
    default:
        av_log(avctx, AV_LOG_ERROR, "Unknown picture type encountered, expect the output to be broken.\n");
        av_log(avctx, AV_LOG_ERROR, "Please report this error and include as much information on how to reproduce it as possible.\n");
        res = AVERROR_EXTERNAL;
        goto error;
    }

#if FF_API_CODED_FRAME
FF_DISABLE_DEPRECATION_WARNINGS
    avctx->coded_frame->pict_type = pict_type;
FF_ENABLE_DEPRECATION_WARNINGS
#endif

    ff_side_data_set_encoder_stats(pkt,
        (lock_params.frameAvgQP - 1) * FF_QP2LAMBDA, NULL, 0, pict_type);

    pkt->pts = lock_params.outputTimeStamp;
    pkt->dts = nvenc_dequeue_timestamp(ctx->timestamps);

    /* when there're b frame(s), set dts offset */
    if (ctx->config.frameIntervalP >= 2)
        pkt->dts -= 1;

    if (pkt->dts > pkt->pts)
        pkt->dts = pkt->pts;

    if (ctx->last_dts != AV_NOPTS_VALUE && pkt->dts <= ctx->last_dts)
        pkt->dts = ctx->last_dts + 1;

    ctx->last_dts = pkt->dts;

    av_free(slice_offsets);

    return 0;

error:

    av_free(slice_offsets);
    nvenc_dequeue_timestamp(ctx->timestamps);

    return res;
}

static int output_ready(NVENCContext *ctx, int flush)
{
    int nb_ready, nb_pending;

    nb_ready   = av_fifo_size(ctx->ready)   / sizeof(NVENCFrame*);
    nb_pending = av_fifo_size(ctx->pending)         / sizeof(NVENCFrame*);
    return nb_ready > 0 && (flush || nb_ready + nb_pending >= ctx->buffer_delay);
}

int ff_nvenc_encode_frame(AVCodecContext *avctx, AVPacket *pkt,
                          const AVFrame *frame, int *got_packet)
{
    NVENCContext *ctx               = avctx->priv_data;
    NV_ENCODE_API_FUNCTION_LIST *nv = &ctx->nvel.nvenc_funcs;
    NV_ENC_PIC_PARAMS params        = { 0 };
    NVENCFrame *tmpoutsurf, *nvenc_frame;
    NVENCSTATUS enc_ret;
    int ret;

    params.version = NV_ENC_PIC_PARAMS_VER;

    if (frame) {
        nvenc_frame = get_free_frame(ctx);
        av_assert0(nvenc_frame);

        ret = nvenc_upload_frame(avctx, frame, nvenc_frame);
        if (ret) {
            nvenc_frame->locked = 0;
            return ret;
        }

        params.inputBuffer     = nvenc_frame->in;
        params.bufferFmt       = nvenc_frame->format;
        params.inputWidth      = avctx->width;
        params.inputHeight     = avctx->height;
        params.outputBitstream = nvenc_frame->out;
        params.inputTimeStamp = frame->pts;
        params.completionEvent = 0;

        if (avctx->flags & AV_CODEC_FLAG_INTERLACED_DCT) {
            if (frame->top_field_first)
                params.pictureStruct = NV_ENC_PIC_STRUCT_FIELD_TOP_BOTTOM;
            else
                params.pictureStruct = NV_ENC_PIC_STRUCT_FIELD_BOTTOM_TOP;
        } else {
            params.pictureStruct = NV_ENC_PIC_STRUCT_FRAME;
        }

        params.encodePicFlags = 0;
        params.inputDuration = 0;

        nvenc_codec_specific_pic_params(avctx, &params);

        nvenc_enqueue_timestamp(ctx->timestamps, frame->pts);
    } else {
        params.encodePicFlags = NV_ENC_PIC_FLAG_EOS;
    }

    enc_ret = nv->nvEncEncodePicture(ctx->nvenc_ctx, &params);

    if (frame && enc_ret == NV_ENC_ERR_NEED_MORE_INPUT)
        av_fifo_generic_write(ctx->pending, &nvenc_frame, sizeof(nvenc_frame), NULL);

    if (enc_ret != NV_ENC_SUCCESS && enc_ret != NV_ENC_ERR_NEED_MORE_INPUT) {
        return nvenc_print_error(avctx, enc_ret, "EncodePicture failed!");
    }

    if (enc_ret != NV_ENC_ERR_NEED_MORE_INPUT) {
        while (av_fifo_size(ctx->pending) > 0) {
            av_fifo_generic_read(ctx->pending, &tmpoutsurf, sizeof(tmpoutsurf), NULL);
            av_fifo_generic_write(ctx->ready, &tmpoutsurf, sizeof(tmpoutsurf), NULL);
        }

        if (frame)
            av_fifo_generic_write(ctx->ready, &nvenc_frame, sizeof(nvenc_frame), NULL);
    }

    if (output_ready(ctx, !frame)) {
        av_fifo_generic_read(ctx->ready, &tmpoutsurf, sizeof(tmpoutsurf), NULL);

        ret = process_out(avctx, pkt, tmpoutsurf);

        if (ret)
            return ret;

        av_assert0(tmpoutsurf->locked);
        tmpoutsurf->locked--;

        *got_packet = 1;
    } else {
        *got_packet = 0;
    }

    return 0;
}
