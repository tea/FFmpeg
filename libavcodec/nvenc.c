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

#if HAVE_WINDOWS_H
#include <windows.h>

#define CUDA_LIBNAME TEXT("nvcuda.dll")
#if ARCH_X86_64
#define NVENC_LIBNAME "nvEncodeAPI64.dll"
#else
#define NVENC_LIBNAME "nvEncodeAPI.dll"
#endif

#define dlopen(filename, flags) LoadLibrary((filename))
#define dlsym(handle, symbol)   GetProcAddress(handle, symbol)
#define dlclose(handle)         FreeLibrary(handle)

#else
#include <dlfcn.h>

#define CUDA_LIBNAME "libcuda.so"
#define NVENC_LIBNAME "libnvidia-encode.so"
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

#define NVENC_CAP 0x30
#define IS_CBR(rc) (rc == NV_ENC_PARAMS_RC_CBR ||               \
                    rc == NV_ENC_PARAMS_RC_2_PASS_QUALITY ||    \
                    rc == NV_ENC_PARAMS_RC_2_PASS_FRAMESIZE_CAP)

#define LOAD_LIBRARY(l, path)                   \
    do {                                        \
        if (!((l) = dlopen(path, RTLD_LAZY))) { \
            av_log(avctx, AV_LOG_ERROR,         \
                   "Cannot load %s\n",          \
                   path);                       \
            res = AVERROR_UNKNOWN;              \
            goto error;                         \
        }                                       \
    } while (0)

#define LOAD_SYMBOL(fun, lib, symbol)        \
    do {                                     \
        if (!((fun) = dlsym(lib, symbol))) { \
            av_log(avctx, AV_LOG_ERROR,      \
                   "Cannot load %s\n",       \
                   symbol);                  \
            res = AVERROR_UNKNOWN;           \
            goto error;                      \
        }                                    \
    } while (0)

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

static av_cold int nvenc_load_libraries(AVCodecContext *avctx)
{
    NVENCContext *ctx         = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;
    PNVENCODEAPICREATEINSTANCE nvenc_create_instance;
    NVENCSTATUS err;
    int res;

#if CONFIG_CUDA
    nvel->cu_init                      = cuInit;
    nvel->cu_device_get_count          = cuDeviceGetCount;
    nvel->cu_device_get                = cuDeviceGet;
    nvel->cu_device_get_name           = cuDeviceGetName;
    nvel->cu_device_compute_capability = cuDeviceComputeCapability;
    nvel->cu_ctx_create                = cuCtxCreate_v2;
    nvel->cu_ctx_pop_current           = cuCtxPopCurrent_v2;
    nvel->cu_ctx_destroy               = cuCtxDestroy_v2;
#else
    LOAD_LIBRARY(nvel->cuda, CUDA_LIBNAME);

    LOAD_SYMBOL(nvel->cu_init, nvel->cuda, "cuInit");
    LOAD_SYMBOL(nvel->cu_device_get_count, nvel->cuda, "cuDeviceGetCount");
    LOAD_SYMBOL(nvel->cu_device_get, nvel->cuda, "cuDeviceGet");
    LOAD_SYMBOL(nvel->cu_device_get_name, nvel->cuda, "cuDeviceGetName");
    LOAD_SYMBOL(nvel->cu_device_compute_capability, nvel->cuda,
                "cuDeviceComputeCapability");
    LOAD_SYMBOL(nvel->cu_ctx_create, nvel->cuda, "cuCtxCreate_v2");
    LOAD_SYMBOL(nvel->cu_ctx_pop_current, nvel->cuda, "cuCtxPopCurrent_v2");
    LOAD_SYMBOL(nvel->cu_ctx_destroy, nvel->cuda, "cuCtxDestroy_v2");
#endif

    LOAD_LIBRARY(nvel->nvenc, NVENC_LIBNAME);

    LOAD_SYMBOL(nvenc_create_instance, nvel->nvenc,
                "NvEncodeAPICreateInstance");

    nvel->nvenc_funcs.version = NV_ENCODE_API_FUNCTION_LIST_VER;

    err = nvenc_create_instance(&nvel->nvenc_funcs);
    if (err != NV_ENC_SUCCESS) {
        nvenc_print_error(avctx, err, "Cannot create the NVENC instance");
        goto error;
    }

    return 0;

error:
    if (nvel->nvenc)
        dlclose(nvel->nvenc);
    nvel->nvenc = NULL;
#if !CONFIG_CUDA
    if (nvel->cuda)
        dlclose(nvel->cuda);
    nvel->cuda = NULL;
#endif

    return res;
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
        return nvenc_print_error(avctx, ret, "Cannot open the NVENC Session");
    }

    return 0;
}

static int nvenc_check_codec_support(AVCodecContext *avctx)
{
    NVENCContext *ctx               = avctx->priv_data;
    NV_ENCODE_API_FUNCTION_LIST *nv = &ctx->nvel.nvenc_funcs;
    int i, ret, count = 0;
    GUID *guids = NULL;

    ret = nv->nvEncGetEncodeGUIDCount(ctx->nvenc_ctx, &count);

    if (ret != NV_ENC_SUCCESS || !count)
        return AVERROR(ENOSYS);

    guids = av_malloc(count * sizeof(GUID));
    if (!guids)
        return AVERROR(ENOMEM);

    ret = nv->nvEncGetEncodeGUIDs(ctx->nvenc_ctx, guids, count, &count);
    if (ret != NV_ENC_SUCCESS) {
        ret = AVERROR(ENOSYS);
        goto fail;
    }

    ret = AVERROR(ENOSYS);
    for (i = 0; i < count; i++) {
        if (!memcmp(&guids[i], &ctx->params.encodeGUID, sizeof(*guids))) {
            ret = 0;
            break;
        }
    }

fail:
    av_free(guids);

    return ret;
}

static int nvenc_check_cap(AVCodecContext *avctx, NV_ENC_CAPS cap)
{
    NVENCContext *ctx               = avctx->priv_data;
    NV_ENCODE_API_FUNCTION_LIST *nv = &ctx->nvel.nvenc_funcs;
    NV_ENC_CAPS_PARAM params        = { 0 };
    int ret, val = 0;

    params.version     = NV_ENC_CAPS_PARAM_VER;
    params.capsToQuery = cap;

    ret = nv->nvEncGetEncodeCaps(ctx->nvenc_ctx, ctx->params.encodeGUID, &params, &val);

    if (ret == NV_ENC_SUCCESS)
        return val;
    return 0;
}

static int nvenc_check_capabilities(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    int ret;

    ret = nvenc_check_codec_support(avctx);
    if (ret < 0) {
        av_log(avctx, AV_LOG_VERBOSE, "Codec not supported\n");
        return ret;
    }

    ret = nvenc_check_cap(avctx, NV_ENC_CAPS_SUPPORT_YUV444_ENCODE);
    if (ctx->data_pix_fmt == AV_PIX_FMT_YUV444P && ret <= 0) {
        av_log(avctx, AV_LOG_VERBOSE, "YUV444P not supported\n");
        return AVERROR(ENOSYS);
    }

    ret = nvenc_check_cap(avctx, NV_ENC_CAPS_WIDTH_MAX);
    if (ret < avctx->width) {
        av_log(avctx, AV_LOG_VERBOSE, "Width %d exceeds %d\n",
               avctx->width, ret);
        return AVERROR(ENOSYS);
    }

    ret = nvenc_check_cap(avctx, NV_ENC_CAPS_HEIGHT_MAX);
    if (ret < avctx->height) {
        av_log(avctx, AV_LOG_VERBOSE, "Height %d exceeds %d\n",
               avctx->height, ret);
        return AVERROR(ENOSYS);
    }

    ret = nvenc_check_cap(avctx, NV_ENC_CAPS_NUM_MAX_BFRAMES);
    if (ret < avctx->max_b_frames) {
        av_log(avctx, AV_LOG_VERBOSE, "Max b-frames %d exceed %d\n",
               avctx->max_b_frames, ret);

        return AVERROR(ENOSYS);
    }

    return 0;
}

static int nvenc_check_device(AVCodecContext *avctx, int idx)
{
    NVENCContext *ctx               = avctx->priv_data;
    NVENCLibraryContext *nvel       = &ctx->nvel;
    char name[128]                  = { 0 };
    int major, minor, ret;
    CUdevice cu_device;
    CUcontext dummy;
    int loglevel = AV_LOG_VERBOSE;

    if (ctx->device == LIST_DEVICES)
        loglevel = AV_LOG_INFO;

    ret = nvel->cu_device_get(&cu_device, idx);
    if (ret != CUDA_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR,
               "Cannot access the CUDA device %d\n",
               idx);
        return -1;
    }

    ret = nvel->cu_device_get_name(name, sizeof(name), cu_device);
    if (ret != CUDA_SUCCESS)
        return -1;

    ret = nvel->cu_device_compute_capability(&major, &minor, cu_device);
    if (ret != CUDA_SUCCESS)
        return -1;

    av_log(avctx, loglevel, "Device %d [%s] ", cu_device, name);

    if (((major << 4) | minor) < NVENC_CAP)
        goto fail;

    ret = nvel->cu_ctx_create(&ctx->cu_context_internal, 0, cu_device);
    if (ret != CUDA_SUCCESS)
        goto fail;

    ctx->cu_context = ctx->cu_context_internal;

    ret = nvel->cu_ctx_pop_current(&dummy);
    if (ret != CUDA_SUCCESS)
        goto fail2;

    if ((ret = nvenc_open_session(avctx)) < 0)
        goto fail2;

    if ((ret = nvenc_check_capabilities(avctx)) < 0)
        goto fail3;

    av_log(avctx, loglevel, "supports NVENC\n");

    if (ctx->device == cu_device || ctx->device == ANY_DEVICE)
        return 0;

fail3:
    nvel->nvenc_funcs.nvEncDestroyEncoder(ctx->nvenc_ctx);
    ctx->nvenc_ctx = NULL;

fail2:
    nvel->cu_ctx_destroy(ctx->cu_context_internal);
    ctx->cu_context_internal = NULL;

fail:
    if (ret != 0)
        av_log(avctx, loglevel, "does not support NVENC (major %d minor %d)\n",
               major, minor);

    return AVERROR(ENOSYS);
}

static av_cold int nvenc_setup_device(AVCodecContext *avctx)
{
    NVENCContext *ctx         = avctx->priv_data;
    NVENCLibraryContext *nvel = &ctx->nvel;

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

    if (avctx->pix_fmt == AV_PIX_FMT_CUDA) {
#if CONFIG_CUDA
        AVHWFramesContext   *frames_ctx;
        AVCUDADeviceContext *device_hwctx;
        int ret;

        if (!avctx->hw_frames_ctx)
            return AVERROR(EINVAL);

        frames_ctx   = (AVHWFramesContext*)avctx->hw_frames_ctx->data;
        device_hwctx = frames_ctx->device_ctx->hwctx;

        ctx->cu_context = device_hwctx->cuda_ctx;

        ret = nvenc_open_session(avctx);
        if (ret < 0)
            return ret;

        ret = nvenc_check_capabilities(avctx);
        if (ret < 0)
            return ret;
#else
        return AVERROR_BUG;
#endif
    } else {
        int i, nb_devices = 0;

        if ((nvel->cu_init(0)) != CUDA_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR,
                   "Cannot init CUDA\n");
            return AVERROR_UNKNOWN;
        }

        if ((nvel->cu_device_get_count(&nb_devices)) != CUDA_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR,
                   "Cannot enumerate the CUDA devices\n");
            return AVERROR_UNKNOWN;
        }


        for (i = 0; i < nb_devices; ++i) {
            if ((nvenc_check_device(avctx, i)) >= 0 && ctx->device != LIST_DEVICES)
                return 0;
        }

        if (ctx->device == LIST_DEVICES)
            return AVERROR_EXIT;

        return AVERROR(ENOSYS);
    }

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
    NV_ENC_RC_PARAMS *rc = &ctx->config.rcParams;

    rc->rateControlMode = NV_ENC_PARAMS_RC_CONSTQP;
    rc->constQP.qpInterB = avctx->global_quality;
    rc->constQP.qpInterP = avctx->global_quality;
    rc->constQP.qpIntra  = avctx->global_quality;

    avctx->qmin = -1;
    avctx->qmax = -1;
}

static av_cold void set_vbr(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    NV_ENC_RC_PARAMS *rc = &ctx->config.rcParams;

    int qp_inter_p;

    if (avctx->qmin >= 0) {
        rc->enableMinQP    = 1;
        rc->minQP.qpInterB = avctx->qmin;
        rc->minQP.qpInterP = avctx->qmin;
        rc->minQP.qpIntra  = avctx->qmin;
    }

    if (avctx->qmax >= 0) {
        rc->enableMaxQP = 1;
        rc->maxQP.qpInterB = avctx->qmax;
        rc->maxQP.qpInterP = avctx->qmax;
        rc->maxQP.qpIntra  = avctx->qmax;
    }

    if (avctx->qmin >= 0 && avctx->qmax >= 0) {
        qp_inter_p = (avctx->qmax + 3 * avctx->qmin) / 4; // biased towards Qmin
    } else {
        qp_inter_p = 26; // default to 26
    }

    rc->enableInitialRCQP = 1;
    rc->initialRCQP.qpInterP  = qp_inter_p;

    if(avctx->i_quant_factor != 0.0 && avctx->b_quant_factor != 0.0) {
        rc->initialRCQP.qpIntra = av_clip(
            qp_inter_p * fabs(avctx->i_quant_factor) + avctx->i_quant_offset, 0, 51);
        rc->initialRCQP.qpInterB = av_clip(
            qp_inter_p * fabs(avctx->b_quant_factor) + avctx->b_quant_offset, 0, 51);
    } else {
        rc->initialRCQP.qpIntra = qp_inter_p;
        rc->initialRCQP.qpInterB = qp_inter_p;
    }
}

static av_cold void set_lossless(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    NV_ENC_RC_PARAMS *rc = &ctx->config.rcParams;

    rc->rateControlMode  = NV_ENC_PARAMS_RC_CONSTQP;
    rc->constQP.qpInterB = 0;
    rc->constQP.qpInterP = 0;
    rc->constQP.qpIntra  = 0;

    avctx->qmin = -1;
    avctx->qmax = -1;
}

static void nvenc_override_rate_control(AVCodecContext *avctx)
{
    NVENCContext *ctx    = avctx->priv_data;

    switch (ctx->rc) {
    case NV_ENC_PARAMS_RC_CONSTQP:
        if (avctx->global_quality < 0) {
            av_log(avctx, AV_LOG_WARNING,
                   "The constant quality rate-control requires "
                   "the 'global_quality' option set.\n");
            return;
        }
        set_constqp(avctx);
        return;
    case NV_ENC_PARAMS_RC_2_PASS_VBR:
    case NV_ENC_PARAMS_RC_VBR:
        if (avctx->qmin < 0 && avctx->qmax < 0) {
            av_log(avctx, AV_LOG_WARNING,
                   "The variable bitrate rate-control requires "
                   "the 'qmin' and/or 'qmax' option set.\n");
            return;
        }
    case NV_ENC_PARAMS_RC_VBR_MINQP:
        if (avctx->qmin < 0) {
            av_log(avctx, AV_LOG_WARNING,
                   "The variable bitrate rate-control requires "
                   "the 'qmin' option set.\n");
            return;
        }
        set_vbr(avctx);
        break;
    case NV_ENC_PARAMS_RC_CBR:
        break;
    case NV_ENC_PARAMS_RC_2_PASS_QUALITY:
    case NV_ENC_PARAMS_RC_2_PASS_FRAMESIZE_CAP:
        if (!(ctx->flags & NVENC_LOWLATENCY)) {
            av_log(avctx, AV_LOG_WARNING,
                   "The multipass rate-control requires "
                   "a low-latency preset.\n");
            return;
        }
    }

    ctx->config.rcParams.rateControlMode = ctx->rc;
}

static av_cold void nvenc_setup_rate_control(AVCodecContext *avctx)
{
    NVENCContext *ctx    = avctx->priv_data;
    NV_ENC_RC_PARAMS *rc = &ctx->config.rcParams;

    if (avctx->bit_rate > 0) {
        rc->averageBitRate = avctx->bit_rate;
    } else if (rc->averageBitRate > 0) {
        rc->maxBitRate = rc->averageBitRate;
    }

    if (avctx->rc_max_rate > 0)
        rc->maxBitRate = avctx->rc_max_rate;

    if (ctx->flags & NVENC_LOSSLESS) {
        set_lossless(avctx);
    } else if (ctx->rc > 0) {
        nvenc_override_rate_control(avctx);
    } else if (avctx->global_quality > 0) {
        set_constqp(avctx);
    } else {
        if (avctx->qmin >= 0 && avctx->qmax >= 0) {
            rc->rateControlMode = NV_ENC_PARAMS_RC_VBR_MINQP;
        } else {
            rc->rateControlMode = NV_ENC_PARAMS_RC_VBR;
        }
	set_vbr(avctx);
    }

    if (avctx->rc_buffer_size > 0) {
        rc->vbvBufferSize = avctx->rc_buffer_size;
    } else if (rc->averageBitRate > 0) {
        rc->vbvBufferSize = 2 * rc->averageBitRate;
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

    if (IS_CBR(cc->rcParams.rateControlMode)) {
        h264->outputBufferingPeriodSEI = 1;
        h264->outputPictureTimingSEI   = 1;
    }

    if (cc->rcParams.rateControlMode == NV_ENC_PARAMS_RC_2_PASS_QUALITY ||
        cc->rcParams.rateControlMode == NV_ENC_PARAMS_RC_2_PASS_FRAMESIZE_CAP ||
        cc->rcParams.rateControlMode == NV_ENC_PARAMS_RC_2_PASS_VBR)
    {
        h264->adaptiveTransformMode = NV_ENC_H264_ADAPTIVE_TRANSFORM_ENABLE;
        h264->fmoMode = NV_ENC_H264_FMO_DISABLE;
    }

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

    if (IS_CBR(cc->rcParams.rateControlMode)) {
        hevc->outputBufferingPeriodSEI = 1;
        hevc->outputPictureTimingSEI   = 1;
    }

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

static av_cold int nvenc_setup_frames(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    int i, res;

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

    for(i = 0; i < ctx->nb_surfaces; ++i) {
        res = nvenc_alloc_surface(avctx, i);
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

av_cold int ff_nvenc_encode_close(AVCodecContext *avctx)
{
    NVENCContext *ctx               = avctx->priv_data;
    NV_ENCODE_API_FUNCTION_LIST *nv = &ctx->nvel.nvenc_funcs;
    int i;

    /* the encoder has to be flushed before it can be closed */
    if (ctx->nvenc_ctx) {
        NV_ENC_PIC_PARAMS params        = { .version        = NV_ENC_PIC_PARAMS_VER,
                                            .encodePicFlags = NV_ENC_PIC_FLAG_EOS };

        nv->nvEncEncodePicture(ctx->nvenc_ctx, &params);
    }

    av_fifo_freep(&ctx->timestamps);
    av_fifo_freep(&ctx->pending);
    av_fifo_freep(&ctx->ready);

    if (ctx->frames) {
        if (avctx->pix_fmt == AV_PIX_FMT_CUDA) {
            for (i = 0; i < ctx->nb_surfaces; ++i) {
                if (ctx->frames[i].in)
                    nv->nvEncUnmapInputResource(ctx->nvenc_ctx, ctx->frames[i].in_map.mappedResource);
            }
        }
        for (i = 0; i < ctx->nb_registered_frames; i++) {
            if (ctx->registered_frames[i].regptr)
                nv->nvEncUnregisterResource(ctx->nvenc_ctx, ctx->registered_frames[i].regptr);
        }
        ctx->nb_registered_frames = 0;

        for (i = 0; i < ctx->nb_surfaces; ++i) {
            if (avctx->pix_fmt != AV_PIX_FMT_CUDA)
                nv->nvEncDestroyInputBuffer(ctx->nvenc_ctx, ctx->frames[i].in);
            av_frame_free(&ctx->frames[i].in_ref);
            nv->nvEncDestroyBitstreamBuffer(ctx->nvenc_ctx, ctx->frames[i].out);
        }
    }
    av_freep(&ctx->frames);

    if (ctx->nvenc_ctx)
        nv->nvEncDestroyEncoder(ctx->nvenc_ctx);
    ctx->nvenc_ctx = NULL;

    if (ctx->cu_context_internal)
        ctx->nvel.cu_ctx_destroy(ctx->cu_context_internal);
    ctx->cu_context = ctx->cu_context_internal = NULL;

    if (ctx->nvel.nvenc)
        dlclose(ctx->nvel.nvenc);
    ctx->nvel.nvenc = NULL;

#if !CONFIG_CUDA
    if (ctx->nvel.cuda)
        dlclose(ctx->nvel.cuda);
    ctx->nvel.cuda = NULL;
#endif

    return 0;
}

av_cold int ff_nvenc_encode_init(AVCodecContext *avctx)
{
    NVENCContext *ctx = avctx->priv_data;
    int ret;

    if (avctx->pix_fmt == AV_PIX_FMT_CUDA) {
        AVHWFramesContext *frames_ctx;
        if (!avctx->hw_frames_ctx) {
            av_log(avctx, AV_LOG_ERROR,
                   "hw_frames_ctx must be set when using GPU frames as input\n");
            return AVERROR(EINVAL);
        }
        frames_ctx = (AVHWFramesContext*)avctx->hw_frames_ctx->data;
        ctx->data_pix_fmt = frames_ctx->sw_format;
    } else {
        ctx->data_pix_fmt = avctx->pix_fmt;
    }

    if ((ret = nvenc_load_libraries(avctx)) < 0)
        return ret;

    if ((ret = nvenc_setup_device(avctx)) < 0)
        return ret;

    if ((ret = nvenc_setup_encoder(avctx)) < 0)
        return ret;

    if ((ret = nvenc_setup_frames(avctx)) < 0)
        return ret;

    if (avctx->flags & AV_CODEC_FLAG_GLOBAL_HEADER) {
        if ((ret = nvenc_setup_extradata(avctx)) < 0)
            return ret;
    }

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
