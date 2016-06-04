#ifndef A53_SPLIT_H
#define A53_SPLIT_H

typedef struct A53SplitContext
{
	const AVClass *class;

	int buffer_size;

	int cc_data_seen;
	AVFifoBuffer *cc_data_fifo;
} A53SplitContext;

#endif // A53_SPLIT_H