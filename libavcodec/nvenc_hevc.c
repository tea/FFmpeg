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

#include "libavutil/internal.h"

#include "avcodec.h"
#include "internal.h"

#include "nvenc.h"

#define OFFSET(x) offsetof(NVENCContext, x)
#define VE AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_ENCODING_PARAM
static const AVOption options[] = {
    { "preset",   "Set the encoding preset",              OFFSET(preset),      AV_OPT_TYPE_INT,    { .i64 = PRESET_HQ }, PRESET_DEFAULT, PRESET_LOSSLESS_HP, VE, "preset" },
    { "default",    "",                                   0,                   AV_OPT_TYPE_CONST,  { .i64 = PRESET_DEFAULT }, 0, 0, VE, "preset" },
    { "hp",         "",                                   0,                   AV_OPT_TYPE_CONST,  { .i64 = PRESET_HP }, 0, 0, VE, "preset" },
    { "hq",         "",                                   0,                   AV_OPT_TYPE_CONST,  { .i64 = PRESET_HQ }, 0, 0, VE, "preset" },
    { "bd",         "",                                   0,                   AV_OPT_TYPE_CONST,  { .i64 = PRESET_BD }, 0, 0, VE, "preset" },
    { "ll",         "low latency",                        0,                   AV_OPT_TYPE_CONST,  { .i64 = PRESET_LOW_LATENCY_DEFAULT }, 0, 0, VE, "preset" },
    { "llhq",       "low latency hq",                     0,                   AV_OPT_TYPE_CONST,  { .i64 = PRESET_LOW_LATENCY_HQ }, 0, 0, VE, "preset" },
    { "llhp",       "low latency hp",                     0,                   AV_OPT_TYPE_CONST,  { .i64 = PRESET_LOW_LATENCY_HP }, 0, 0, VE, "preset" },
    { "lossless",   "lossless",                           0,                   AV_OPT_TYPE_CONST,  { .i64 = PRESET_LOSSLESS_DEFAULT }, 0, 0, VE, "preset" },
    { "losslesshp", "lossless hp",                        0,                   AV_OPT_TYPE_CONST,  { .i64 = PRESET_LOSSLESS_HP }, 0, 0, VE, "preset" },
    { "profile", "Set the encoding profile",             OFFSET(profile),      AV_OPT_TYPE_INT,    { .i64 = FF_PROFILE_HEVC_MAIN }, FF_PROFILE_HEVC_MAIN, FF_PROFILE_HEVC_MAIN, VE, "profile" },
    { "high",    "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = FF_PROFILE_HEVC_MAIN }, 0, 0, VE, "profile" },
    { "level",   "Set the encoding level restriction",   OFFSET(level),        AV_OPT_TYPE_INT,    { .i64 = NV_ENC_LEVEL_AUTOSELECT }, NV_ENC_LEVEL_AUTOSELECT, NV_ENC_LEVEL_HEVC_62, VE, "level" },
    { "1",       "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_1 },  0, 0, VE,  "level" },
    { "1.0",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_1 },  0, 0, VE,  "level" },
    { "2",       "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_2 },  0, 0, VE,  "level" },
    { "2.0",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_2 },  0, 0, VE,  "level" },
    { "2.1",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_21 }, 0, 0, VE,  "level" },
    { "3",       "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_3 },  0, 0, VE,  "level" },
    { "3.0",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_3 },  0, 0, VE,  "level" },
    { "3.1",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_31 }, 0, 0, VE,  "level" },
    { "4",       "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_4 },  0, 0, VE,  "level" },
    { "4.0",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_4 },  0, 0, VE,  "level" },
    { "4.1",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_41 }, 0, 0, VE,  "level" },
    { "5",       "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_5 },  0, 0, VE,  "level" },
    { "5.0",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_5 },  0, 0, VE,  "level" },
    { "5.1",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_51 }, 0, 0, VE,  "level" },
    { "5.2",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_52 }, 0, 0, VE,  "level" },
    { "6",       "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_6 },  0, 0, VE,  "level" },
    { "6.0",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_6 },  0, 0, VE,  "level" },
    { "6.1",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_61 }, 0, 0, VE,  "level" },
    { "6.2",     "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_LEVEL_HEVC_62 }, 0, 0, VE,  "level" },
    { "tier",    "Set the encoding tier",                OFFSET(tier),         AV_OPT_TYPE_INT,    { .i64 = NV_ENC_TIER_HEVC_MAIN }, NV_ENC_TIER_HEVC_MAIN, NV_ENC_TIER_HEVC_HIGH, VE, "tier"},
    { "main",    "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_TIER_HEVC_MAIN }, 0, 0, VE, "tier" },
    { "high",    "",                                     0,                    AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_TIER_HEVC_HIGH }, 0, 0, VE, "tier" },
    { "rc",      "Override the preset rate-control",     OFFSET(rc),           AV_OPT_TYPE_INT,    { .i64 = -1 },                   -1, INT_MAX, VE, "rc" },
    { "constqp",          "Constant QP mode",                                                            0, AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_PARAMS_RC_CONSTQP },              0, 0, VE, "rc" },
    { "vbr",              "Variable bitrate mode",                                                       0, AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_PARAMS_RC_VBR },                  0, 0, VE, "rc" },
    { "cbr",              "Constant bitrate mode",                                                       0, AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_PARAMS_RC_CBR },                  0, 0, VE, "rc" },
    { "vbr_minqp",        "Variable bitrate mode with MinQP",                                            0, AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_PARAMS_RC_VBR_MINQP },            0, 0, VE, "rc" },
    { "ll_2pass_quality", "Multi-pass optimized for image quality (only for low-latency presets)",       0, AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_PARAMS_RC_2_PASS_QUALITY },       0, 0, VE, "rc" },
    { "ll_2pass_size",    "Multi-pass optimized for constant frame size (only for low-latency presets)", 0, AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_PARAMS_RC_2_PASS_FRAMESIZE_CAP }, 0, 0, VE, "rc" },
    { "vbr_2pass",        "Multi-pass variable bitrate mode",                                            0, AV_OPT_TYPE_CONST,  { .i64 = NV_ENC_PARAMS_RC_2_PASS_VBR },           0, 0, VE, "rc" },
    { "gpu", "Selects which NVENC capable GPU to use. First GPU is 0, second is 1, and so on.", OFFSET(gpu), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, INT_MAX, VE },
    { "delay", "Delays frame output by the given amount of frames.", OFFSET(buffer_delay), AV_OPT_TYPE_INT, { .i64 = INT_MAX }, 0, INT_MAX, VE },
    { NULL }
};

static const AVCodecDefault nvenc_defaults[] = {
    { "b", "2M" },
    { "qmin", "-1" },
    { "qmax", "-1" },
    { "qdiff", "-1" },
    { "qblur", "-1" },
    { "qcomp", "-1" },
    { "g", "250" },
    { "bf", "0" },
    { NULL },
};

#if CONFIG_NVENC_HEVC_ENCODER
static const AVClass nvenc_hevc_class = {
    .class_name = "nvenc_hevc",
    .item_name = av_default_item_name,
    .option = options,
    .version = LIBAVUTIL_VERSION_INT,
};

AVCodec ff_nvenc_hevc_encoder = {
    .name = "nvenc_hevc",
    .long_name = NULL_IF_CONFIG_SMALL("NVIDIA NVENC hevc encoder"),
    .type = AVMEDIA_TYPE_VIDEO,
    .id = AV_CODEC_ID_H265,
    .priv_data_size = sizeof(NVENCContext),
    .init = ff_nvenc_encode_init,
    .encode2 = ff_nvenc_encode_frame,
    .close = ff_nvenc_encode_close,
    .capabilities = AV_CODEC_CAP_DELAY,
    .priv_class = &nvenc_hevc_class,
    .defaults = nvenc_defaults,
    .pix_fmts = ff_nvenc_pix_fmts,
};
#endif
