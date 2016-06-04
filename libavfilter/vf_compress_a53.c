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

/**
 * @file
 * Collect A53 CC data from several consecutive frames and place it all into each Nth output frame
 */

#include "avfilter.h"
#include "formats.h"
#include "libavutil/fifo.h"
#include "libavutil/common.h"
#include "libavutil/eval.h"
#include "libavutil/avstring.h"
#include "libavutil/pixdesc.h"
#include "libavutil/imgutils.h"
#include "libavutil/mathematics.h"
#include "libavutil/opt.h"
#include "libavutil/timestamp.h"
#include "internal.h"
#include "dualinput.h"
#include "drawutils.h"
#include "video.h"

typedef struct CompressA53Context {
    const AVClass *class;

    int ratio;

    int current_frame;

    AVFifoBuffer *cc_data_fifo;
} CompressA53Context;

static int write_to_fifo(AVFifoBuffer *fifo, void* data, size_t size)
{
    int ret;

    if (av_fifo_space(fifo) < size &&
        (ret = av_fifo_realloc2(fifo, FFMAX(av_fifo_size(fifo) + size, 2*av_fifo_size(fifo))))) {
        return ret;
    }

    av_fifo_generic_write(fifo, data, size, NULL);
    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    CompressA53Context *s = ctx->priv;

    av_fifo_freep(&s->cc_data_fifo);
}


static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    CompressA53Context *s = ctx->priv;
    AVFrameSideData *side_data = av_frame_get_side_data(in, AV_FRAME_DATA_A53_CC);
    if (side_data)
    {
	unsigned int i;
	for(i = 0; i + 2 < side_data->size; i += 3)
        {
             if ((side_data->data[i] & 0xFC) == 0xFC)
                 write_to_fifo(s->cc_data_fifo, side_data->data + i, 3);
        }
        av_frame_remove_side_data(in, AV_FRAME_DATA_A53_CC);
    }

    if (++s->current_frame >= s->ratio)
    {
        s->current_frame = 0;
        if (av_fifo_size(s->cc_data_fifo) > 0)
        {
            unsigned int len = av_fifo_size(s->cc_data_fifo);
            if (len > 60)
                len = 60;
            side_data = av_frame_new_side_data(in, AV_FRAME_DATA_A53_CC, len);
            av_fifo_generic_read(s->cc_data_fifo, side_data->data, side_data->size, NULL);
            if (av_fifo_size(s->cc_data_fifo) > 960)
            {
                av_log(ctx, AV_LOG_WARNING, "CC data FIFO overflown, dropping some CC data...\n");
                av_fifo_reset(s->cc_data_fifo);
            }
        }
    }

    return ff_filter_frame(ctx->outputs[0], in);
}

static av_cold int init(AVFilterContext *ctx)
{
    CompressA53Context *s = ctx->priv;

    s->cc_data_fifo = av_fifo_alloc(64);
    s->current_frame = 0;

    return 0;
}

static int config_input(AVFilterLink *inlink)
{
    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    outlink->w = ctx->inputs[0]->w;
    outlink->h = ctx->inputs[0]->h;
    outlink->time_base = ctx->inputs[0]->time_base;

    return 0;
}


#define OFFSET(x) offsetof(CompressA53Context, x)
#define V AV_OPT_FLAG_VIDEO_PARAM
#define F AV_OPT_FLAG_FILTERING_PARAM
static const AVOption compress_a53_options[] = {
    { "ratio", "Number of consecutive frames to process at once", OFFSET(ratio), AV_OPT_TYPE_INT, { .i64 = 1 }, 0, INT_MAX, V|F },
    { NULL }
};

AVFILTER_DEFINE_CLASS(compress_a53);

static const AVFilterPad avfilter_vf_compress_a53_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad avfilter_vf_compress_a53_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter ff_vf_compress_a53 = {
    .name          = "compress_a53",
    .description   = NULL_IF_CONFIG_SMALL("Combine A53 CC data from second input with video from first input"),
    .init          = init,
    .uninit        = uninit,
    .priv_size     = sizeof(CompressA53Context),
    .priv_class    = &compress_a53_class,
    .inputs        = avfilter_vf_compress_a53_inputs,
    .outputs       = avfilter_vf_compress_a53_outputs,
};
