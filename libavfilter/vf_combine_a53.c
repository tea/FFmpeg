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
 * Attach A53 CC data from second input to video frames from first input
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

typedef struct CombineA53Context {
    const AVClass *class;

    AVFifoBuffer *main_fifo, *cc_frame_fifo, *cc_data_fifo;

} CombineA53Context;

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
    CombineA53Context *s = ctx->priv;

    av_fifo_freep(&s->main_fifo);
    av_fifo_freep(&s->cc_frame_fifo);
    av_fifo_freep(&s->cc_data_fifo);
}

static AVFrame *copy_cc_data(AVFilterContext *ctx)
{
    CombineA53Context *s = ctx->priv;
    AVFrame *main_frame, *cc_frame;
    double main_pts;

    if (av_fifo_size(s->main_fifo) <= 0)
        return NULL;
    av_fifo_generic_peek(s->main_fifo, &main_frame, sizeof(main_frame), NULL);
    main_pts = av_q2d(ctx->inputs[0]->time_base) * main_frame->pts;

    while(av_fifo_size(s->cc_frame_fifo) > 0) {
        AVFrameSideData *side_data;
        double cc_pts;

        av_fifo_generic_peek(s->cc_frame_fifo, &cc_frame, sizeof(cc_frame), NULL);
        cc_pts = av_q2d(ctx->inputs[1]->time_base) * cc_frame->pts;
        if (main_pts < cc_pts) {
            av_fifo_generic_read(s->main_fifo, &main_frame, sizeof(main_frame), NULL);
            av_frame_remove_side_data(main_frame, AV_FRAME_DATA_A53_CC);
            if (av_fifo_size(s->cc_data_fifo) > 0) {
                side_data = av_frame_new_side_data(main_frame, AV_FRAME_DATA_A53_CC, av_fifo_size(s->cc_data_fifo));
                av_fifo_generic_read(s->cc_data_fifo, side_data->data, side_data->size, NULL);
            } else {
	    }
            return main_frame;
        }

        av_fifo_generic_read(s->cc_frame_fifo, &cc_frame, sizeof(cc_frame), NULL);
        side_data = av_frame_get_side_data(cc_frame, AV_FRAME_DATA_A53_CC);
        if (side_data) {
            write_to_fifo(s->cc_data_fifo, side_data->data, side_data->size);
        }
        av_frame_free(&cc_frame);
    }

    return NULL;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFrame *out;
    AVFilterContext *ctx = inlink->dst;
    CombineA53Context *s = ctx->priv;
    if (inlink == ctx->inputs[0]) {
        write_to_fifo(s->main_fifo, &in, sizeof(in));
    } else {
        write_to_fifo(s->cc_frame_fifo, &in, sizeof(in));
    }
    while((out = copy_cc_data(ctx)) != NULL) {
        int ret = ff_filter_frame(ctx->outputs[0], out);
        if (ret < 0) {
            av_frame_free(&out);
            return ret;
        }
    }
    return 0;
}

static int request_frame(AVFilterLink *outlink)
{
    int ret = ff_request_frame(outlink->src->inputs[0]);
    if (ret < 0)
        return ret;
    return ff_request_frame(outlink->src->inputs[1]);

}

static av_cold int init(AVFilterContext *ctx)
{
    CombineA53Context *s = ctx->priv;

    s->main_fifo = av_fifo_alloc_array(4, sizeof(AVFrame*));
    s->cc_frame_fifo = av_fifo_alloc_array(4, sizeof(AVFrame*));
    s->cc_data_fifo = av_fifo_alloc(64);
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


static const AVOption combine_a53_options[] = {
    { NULL }
};

AVFILTER_DEFINE_CLASS(combine_a53);

static const AVFilterPad avfilter_vf_combine_a53_inputs[] = {
    {
        .name         = "main",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
        .config_props = config_input,
    },
    {
        .name         = "combine_a53",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
        .config_props = config_input,
    },
    { NULL }
};

static const AVFilterPad avfilter_vf_combine_a53_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
	.request_frame = request_frame,
        .config_props  = config_output,
    },
    { NULL }
};

AVFilter ff_vf_combine_a53 = {
    .name          = "combine_a53",
    .description   = NULL_IF_CONFIG_SMALL("Combine A53 CC data from second input with video from first input"),
    .init          = init,
    .uninit        = uninit,
    .priv_size     = sizeof(CombineA53Context),
    .priv_class    = &combine_a53_class,
    .inputs        = avfilter_vf_combine_a53_inputs,
    .outputs       = avfilter_vf_combine_a53_outputs,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL,
};
