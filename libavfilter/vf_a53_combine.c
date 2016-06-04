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
* Recombine A53 CC data from a53_split to video frames
*/

#include "avfilter.h"
#include "formats.h"
#include "libavutil/fifo.h"
#include "libavutil/common.h"
#include "libavutil/opt.h"
#include "internal.h"
#include "video.h"
#include "vf_a53_split.h"

typedef struct A53CombineContext {
	const AVClass *class;

	int ratio;
	int cc_rate;

	int current_frame;
	A53SplitContext *split_context;
} A53CombineContext;

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
	AVFilterContext *ctx = inlink->dst;
	A53CombineContext *s = ctx->priv;
	AVFrameSideData *side_data = av_frame_get_side_data(in, AV_FRAME_DATA_A53_CC);
	int send_cc = 0;

	if (!s->split_context)
	{
		for (AVFilterContext *cctx = ctx; cctx && cctx->nb_inputs == 1; cctx = cctx->inputs[0]->src)
		{
			if (strcmp(cctx->filter->name, "a53_split") == 0)
			{
				s->split_context = cctx->priv;
				break;
			}
		}
		if (!s->split_context)
		{
			av_log(ctx, AV_LOG_ERROR, "Cannot find parent a53_split filter instance\n");
			return AVERROR(EINVAL);
		}
	}

	if (side_data)
	{
		av_log(ctx, AV_LOG_WARNING, "CC data found in input frame after a53_split!\n");
		av_frame_remove_side_data(in, AV_FRAME_DATA_A53_CC);
	}

	if (++s->current_frame >= s->ratio)
	{
		send_cc = 1;
		s->current_frame = 0;
	}

	if (s->split_context->cc_data_seen)
	{
		unsigned int len = send_cc ? av_fifo_size(s->split_context->cc_data_fifo)/3 : 0;
		if (len > s->cc_rate)
			len = s->cc_rate;

		side_data = av_frame_new_side_data(in, AV_FRAME_DATA_A53_CC, s->cc_rate * 3);
		if (len > 0)
			av_fifo_generic_read(s->split_context->cc_data_fifo, side_data->data, len * 3, NULL);

		for(; len < s->cc_rate; ++len)
		{
			side_data->data[3 * len] = 0xFA;
			side_data->data[3 * len + 1] = 0;
			side_data->data[3 * len + 2] = 0;
		}
	}

	return ff_filter_frame(ctx->outputs[0], in);
}

static av_cold int init(AVFilterContext *ctx)
{
	A53CombineContext *s = ctx->priv;

	s->split_context = NULL;
	s->current_frame = 0;

	return 0;
}


#define OFFSET(x) offsetof(A53CombineContext, x)
#define V AV_OPT_FLAG_VIDEO_PARAM
#define F AV_OPT_FLAG_FILTERING_PARAM
static const AVOption a53_combine_options[] = {
	{ "ratio", "Output CC data in each Nth frame",                OFFSET(ratio), AV_OPT_TYPE_INT, {.i64 =  1 }, 0, INT_MAX, V | F },
	{ "cc_rate", "Number of CC frames to output in each frame", OFFSET(cc_rate), AV_OPT_TYPE_INT, {.i64 = 20 }, 0,      31, V | F },
	{ NULL }
};

AVFILTER_DEFINE_CLASS(a53_combine);

static const AVFilterPad avfilter_vf_a53_combine_inputs[] = {
	{
		.name = "default",
		.type = AVMEDIA_TYPE_VIDEO,
		.filter_frame = filter_frame,
	},
	{ NULL }
};

static const AVFilterPad avfilter_vf_a53_combine_outputs[] = {
	{
		.name = "default",
		.type = AVMEDIA_TYPE_VIDEO,
	},
	{ NULL }
};

AVFilter ff_vf_a53_combine = {
	.name = "a53_combine",
	.description = NULL_IF_CONFIG_SMALL("Recombine A53 CC data from a53_split to video frames"),
	.init = init,
	.priv_size = sizeof(A53CombineContext),
	.priv_class = &a53_combine_class,
	.inputs = avfilter_vf_a53_combine_inputs,
	.outputs = avfilter_vf_a53_combine_outputs,
};
