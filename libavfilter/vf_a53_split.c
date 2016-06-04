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
 * Split A5 CC data from video frames to a fifo
 */

#include "avfilter.h"
#include "formats.h"
#include "libavutil/fifo.h"
#include "libavutil/common.h"
#include "libavutil/opt.h"
#include "internal.h"
#include "video.h"
#include "vf_a53_split.h"

static av_cold void uninit(AVFilterContext *ctx)
{
    A53SplitContext *s = ctx->priv;

    av_fifo_freep(&s->cc_data_fifo);
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
	AVFilterContext *ctx = inlink->dst;
	A53SplitContext *s = ctx->priv;
	AVFrameSideData *side_data = av_frame_get_side_data(in, AV_FRAME_DATA_A53_CC);
	if (side_data)
	{
		s->cc_data_seen = 1;
		for (unsigned int i = 0; i + 2 < side_data->size; i += 3)
		{
			if ((side_data->data[i] & 0xFC) == 0xFC)
			{
				if (av_fifo_space(s->cc_data_fifo) < 3)
				{
					av_log(ctx, AV_LOG_WARNING, "CC data FIFO overflown, flushing CC data...\n");
					av_fifo_reset(s->cc_data_fifo);
				}
				av_fifo_generic_write(s->cc_data_fifo, side_data->data + i, 3, NULL);
			}
		}
		av_frame_remove_side_data(in, AV_FRAME_DATA_A53_CC);
	}

	return ff_filter_frame(ctx->outputs[0], in);
}

static av_cold int init(AVFilterContext *ctx)
{
    A53SplitContext *s = ctx->priv;

	s->cc_data_fifo = av_fifo_alloc(s->buffer_size);
	s->cc_data_seen = 0;
    return 0;
}

#define OFFSET(x) offsetof(A53SplitContext, x)
#define V AV_OPT_FLAG_VIDEO_PARAM
#define F AV_OPT_FLAG_FILTERING_PARAM
static const AVOption a53_split_options[] = {
	{ "buffer_size", "Maximum number of A53 frames to store in FIFO", OFFSET(buffer_size), AV_OPT_TYPE_INT,{ .i64 = 1000 }, 0, INT_MAX, V | F },
	{ NULL }
};
AVFILTER_DEFINE_CLASS(a53_split);

static const AVFilterPad avfilter_vf_a53_split_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
    },
	{ NULL }
};

static const AVFilterPad avfilter_vf_a53_split_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter ff_vf_a53_split = {
    .name          = "a53_split",
    .description   = NULL_IF_CONFIG_SMALL("Separate CC data from video frames and store them for a53_combine"),
    .init          = init,
    .uninit        = uninit,
    .priv_size     = sizeof(A53SplitContext),
    .priv_class    = &a53_split_class,
    .inputs        = avfilter_vf_a53_split_inputs,
    .outputs       = avfilter_vf_a53_split_outputs,
};
