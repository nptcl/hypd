#include "deflate_encode.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 *  Error
 */
void deflate_error(const char *x)
{
	fprintf(stderr, "[ERROR] %s\n", x); \
}
#define DeflateError(x) deflate_error(x)
#define DeflateError1(x,y) { \
	char __message[256]; \
	snprintf(__message, 256, x, y); \
	DeflateError(__message); \
}
#ifdef HYPD_DEBUG
#define DeflateDebug(x, y) { \
	if (x) { \
		DeflateError(y); \
		exit(1); \
	} \
}
#else
#define DeflateDebug(x, y)
#endif


/*
 *  Info
 */
#ifdef HYPD_INFO
#define DEFLATE_INFO
#endif

#ifdef DEFLATE_INFO
#include <ctype.h>
#include <stdarg.h>

#define DeflateState(x) { \
	fprintf(stderr, "Deflate.state: %s\n", (x)); \
	fflush(stderr); \
}
void deflate_info(const char *str, ...)
{
	va_list list;

	va_start(list, str);
	fprintf(stderr, "Deflate.");
	vfprintf(stderr, str, list);
	va_end(list);
	fflush(NULL);
}

/* #define DeflateInfo(...) deflate_info(__VA_ARGS__) */
#define DeflateInfo1(x) deflate_info(x)
#define DeflateInfo2(x,a) deflate_info(x,a)
#define DeflateInfo3(x,a,b) deflate_info(x,a,b)
#define DeflateInfo4(x,a,b,c) deflate_info(x,a,b,c)
#define DeflateInfo5(x,a,b,c,d) deflate_info(x,a,b,c,d)
#define DeflateInfo6(x,a,b,c,d,e) deflate_info(x,a,b,c,d,e)
#else
#define DeflateState(x)
/* #define DeflateInfo(x) */
#define DeflateInfo1(x)
#define DeflateInfo2(x,a)
#define DeflateInfo3(x,a,b)
#define DeflateInfo4(x,a,b,c)
#define DeflateInfo5(x,a,b,c,d)
#define DeflateInfo6(x,a,b,c,d,e)
#endif

#define DeflateOutputSpace(x)	(DEFLATE_RING2 - (x))
#define DeflateOutputEmpty(x)	((x) < DEFLATE_RING2)
#define DeflateOutputExist(x)	((x) != 0)
#define DeflateOutputFull(x)	(DEFLATE_RING2 <= (x))

void deflate_init(struct deflate *encode)
{
#ifdef HYPD_DEBUG
	memset(encode, 0xAA, sizeof(struct deflate));
#endif
	encode->input_call = 0;
	encode->output_call = 0;
	encode->flush = 0;
	encode->lz77_disable = 0;
	encode->inputa = 0;
	encode->inputb = 0;
	encode->inputc = 0;
	encode->outputa = 0;
	encode->outputb = 0;
	encode->outputc = 0;
	encode->outputx = 0;
	encode->outputv = 0;
	encode->lz77now = 0;
	encode->lz77size = 0;
	encode->input_callback = NULL;
	encode->input_instance = NULL;
	/* encode->mode = deflate_mode_default; */
	encode->mode = deflate_mode_plane_only;

	/* next */
	encode->state = Deflate_start;
}


/*
 *  Ring
 */
const void *deflate_input(struct deflate *encode,
		const void *ptr, size_t size, size_t *ret)
{
	char *output;
	const char *input;
	size_t x, y, z, b, c;

	/* end-of-file */
	if (encode->end_of_file) {
		DeflateError("Input buffer is already EOF.");
		*ret = 0;
		return NULL;
	}

	/* zero */
	if (size == 0) {
		*ret = 0;
		return ptr;
	}

	/* full */
	c = encode->inputc;
	if (DEFLATE_RING1 <= c) {
		*ret = 0;
		return ptr;
	}

	/* size */
	x = DEFLATE_RING1 - c;
	if (size < x)
		x = size;

	/* body */
	input = (const char *)ptr;
	output = (char *)(encode->input);
	b = encode->inputb;
	z = b + x;
	if (z < DEFLATE_RING1) {
		memcpy(output + b, input, x);
	}
	else {
		y = DEFLATE_RING1 - b;
		z = x - y;
		memcpy(output + b, input, y);
		memcpy(output, input + y, z);
	}
	encode->inputb = z;
	input += x;

	/* result */
	encode->inputc += x;
	*ret = x;
	return (const void *)input;
}

void *deflate_output(struct deflate *encode,
		void *ptr, size_t size, size_t *ret)
{
	char *output;
	const char *input;
	size_t a, c, x, y, z;

	/* zero */
	if (size == 0) {
		*ret = 0;
		return ptr;
	}

	/* empty */
	c = encode->outputc;
	if (c == 0) {
		*ret = 0;
		return ptr;
	}

	/* size */
	x = (size < c)? size: c;

	/* body */
	output = (char *)ptr;
	input = (const char *)(encode->output);
	a = encode->outputa;
	z = x + a;
	if (z < DEFLATE_RING2) {
		memcpy(output, input + a, x);
	}
	else {
		y = DEFLATE_RING2 - a;
		z = x - y;
		memcpy(output, input + a, y);
		memcpy(output + y, input, z);
	}
	encode->outputa = z;
	output += x;

	/* result */
	encode->outputc -= x;
	*ret = x;
	return (void *)output;
}

static void deflate_inputa_forward(struct deflate *encode, unsigned byte_size)
{
	DeflateDebug(encode->inputc < byte_size, "inputc error.");
	encode->inputc -= byte_size;
	encode->inputa += byte_size;
	if (DEFLATE_RING1 <= encode->inputa)
		encode->inputa = encode->inputa % DEFLATE_RING1;
}

static void deflate_outputb_forward(struct deflate *encode, unsigned byte_size)
{
	DeflateDebug(DEFLATE_RING2 < (encode->outputc + byte_size), "outputc error.");
	encode->outputc += byte_size;
	encode->outputb += byte_size;
	if (DEFLATE_RING2 <= encode->outputb)
		encode->outputb = encode->outputb % DEFLATE_RING2;
}


/*
 *  Read
 */
static int deflate_read(struct deflate *encode, int *ret)
{
	uint8_t v;
	deflate_callback callback;

	/* return buffer */
	if (encode->inputc != 0) {
		v = encode->input[encode->inputa];
		deflate_inputa_forward(encode, 1);

		/* lz77 */
		encode->lz77[encode->lz77now] = v;
		if (encode->lz77size < DEFLATE_LZ77)
			encode->lz77size++;
		encode->lz77now++;
		if (DEFLATE_LZ77 <= encode->lz77now)
			encode->lz77now = 0;

		/* checksum */
		callback = encode->input_callback;
		if (callback)
			(*callback)(encode->input_instance, v);

		/* result */
		*ret = v;
		return 0;
	}
	else {
		/* end-of-file */
		if (encode->end_of_file) {
			*ret = -1;
			return 0;
		}

		/* input call */
		*ret = 0;
		return 1;
	}
}


/*
 *  Write
 */
static int deflate_write_p(struct deflate *encode, unsigned bit_size)
{
	unsigned c, x;

	if (bit_size == 0)
		return 1;
	c = DeflateOutputSpace(encode->outputc);
	x = c * 8U - encode->outputx;
	return bit_size <= x;
}

int deflate_alignment(struct deflate *encode)
{
	if (encode->outputx == 0)
		return 0;
	if (DeflateOutputFull(encode->outputc))
		return 1;
	encode->output[encode->outputb] = encode->outputv;
	deflate_outputb_forward(encode, 1);
	encode->outputv = 0;
	encode->outputx = 0;

	return 0;
}

static int deflate_write_1(struct deflate *encode, int value)
{
	unsigned x;

	x = encode->outputx;
	DeflateDebug(8U <= x, "alignment error.");
	if (x < 7U) {
		if (value)
			encode->outputv |= (1 << x);
		encode->outputx++;
	}
	else {
		if (DeflateOutputFull(encode->outputc))
			return 1;
		if (value)
			encode->outputv |= (1 << x);
		encode->output[encode->outputb] = encode->outputv;
		deflate_outputb_forward(encode, 1);
		encode->outputv = 0;
		encode->outputx = 0;
	}

	return 0;
}

static void deflate_write_1_unsafe(struct deflate *encode, int value)
{
	if (deflate_write_1(encode, value))
		DeflateError("index error");
}

static void deflate_write_8_unsafe(struct deflate *encode, uint8_t value)
{
	DeflateDebug(encode->outputx, "outputx error.");
	DeflateDebug(DeflateOutputFull(encode->outputc), "outputc error.");
	encode->output[encode->outputb] = value;
	deflate_outputb_forward(encode, 1);
}

static int deflate_write_8(struct deflate *encode, uint8_t value)
{
	if (DeflateOutputFull(encode->outputc)) {
		return 1;
	}
	else {
		deflate_write_8_unsafe(encode, value);
		return 0;
	}
}

static void deflate_write_16_unsafe(struct deflate *encode, uint16_t value)
{
	deflate_write_8_unsafe(encode, 0xFFU & value);
	deflate_write_8_unsafe(encode, 0xFFU & (value >> 8U));
}

int deflate_byte_write_p(struct deflate *encode, unsigned size)
{
	if (encode->outputx)
		return -1;
	if (DeflateOutputSpace(encode->outputc) < size)
		return 1;
	else
		return 0;
}

int deflate_byte_putc(struct deflate *encode, uint8_t value)
{
	if (encode->outputx)
		return -1;
	if (DeflateOutputFull(encode->outputc))
		return 1;
	else
		return deflate_write_8(encode, value);
}


/*
 *  Start
 */
static int deflate_switch(struct deflate *encode)
{
	DeflateState("switch");
	switch (encode->mode) {
		case deflate_mode_plane_only:
			encode->state = Deflate_plane;
			return 0;

		case deflate_mode_fixed_only:
		case deflate_mode_dynamic_only:
		case deflate_mode_fast:
		case deflate_mode_default:
		case deflate_mode_slow:
			DeflateError("TODO");
			return -1;

		default:
			DeflateError("mode error.");
			return -1;
	}
}

static int deflate_start(struct deflate *encode)
{
	DeflateState("start");
	encode->end_of_file = 0;
	return deflate_switch(encode);
}


/*
 *  Plane
 */
static int deflate_plane(struct deflate *encode)
{
	DeflateState("plane");
	encode->work_now = 0;
	encode->work_size = 0;

	/* next */
#ifdef HYPD_DEBUG
	encode->state_index = 0;
#endif
	encode->state = Deflate_plane_loop;
	return 0;
}

static int deflate_plane_loop(struct deflate *encode)
{
	uint8_t *work;
	int value, check;
	unsigned work_size;

#ifdef HYPD_DEBUG
	if (encode->state_index == 0) {
		DeflateState("plane_loop");
		encode->state_index = 1;
	}
#endif
	work = encode->work;
	work_size = encode->work_size;
	check = 0;
	for (;;) {
		/* full buffer */
		if (DEFLATE_PLANE <= work_size) {
			encode->state = Deflate_plane_header;
			break;
		}

		/* input call */
		if (deflate_read(encode, &value)) {
			encode->input_call = 1;
			check = 1;
			break;
		}

		/* end-of-file */
		if (value < 0) {
			encode->state = work_size?
				Deflate_plane_header:
				Deflate_flush;
			break;
		}

		/* copy */
		work[work_size] = (uint8_t)value;
		work_size++;
	}

	/* next */
	encode->work_size = work_size;
	return check;
}

static int deflate_plane_data(struct deflate *encode)
{
	uint8_t c;
	uint8_t *work;
	uint16_t x, y;
	int check;
	unsigned work_now, work_size;

#ifdef HYPD_DEBUG
	if (encode->state_index == 0) {
		DeflateState("plane_data");
	}
#endif
	switch (encode->state_index) {
		case 0: goto plane_alignment;
		case 1: goto plane_size;
		case 2: goto plane_data;
		default: break;
	}
	DeflateError("state_index error.");
	return -1;

plane_alignment:
	check = deflate_alignment(encode);
	if (check < 0) {
		DeflateError("defliate_alignment error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}
	encode->state_index = 1;

plane_size:
	DeflateDebug(encode->outputx, "outputx error.");
	DeflateDebug(DEFLATE_PLANE < encode->work_size, "work_size error.");
	if (DeflateOutputSpace(encode->outputc) < 4U) {
		encode->output_call = 1;
		return 1;
	}
	x = (uint16_t)encode->work_size;
	y = ~x;
	deflate_write_16_unsafe(encode, x);
	deflate_write_16_unsafe(encode, y);
	encode->state_index = 2;

plane_data:
	work = encode->work;
	work_now = encode->work_now;
	work_size = encode->work_size;
	while (work_now < work_size) {
		c = work[work_now];
		if (deflate_write_8(encode, c)) {
			encode->work_now = work_now;
			encode->output_call = 1;
			return 1;
		}
		work_now++;
	}

	/* next */
	encode->work_now = 0;
	encode->work_size = 0;
	encode->state_index = 0;
	encode->state = Deflate_switch;
	return 0;
}

static int deflate_plane_header(struct deflate *encode)
{
	int eof;

	DeflateState("plane_header");

	/* input */
	if (! deflate_write_p(encode, 3)) {
		encode->output_call = 1;
		return 1;
	}

	/* finalbit */
	eof = encode->end_of_file;
	deflate_write_1_unsafe(encode, eof);

	/* plane */
	deflate_write_1_unsafe(encode, 0);
	deflate_write_1_unsafe(encode, 0);

	/* next */
	encode->state_index = 0;
	encode->state = Deflate_plane_data;
	return 0;
}

static int deflate_flush(struct deflate *encode)
{
	DeflateState("flush");

	/* outputv */
	if (deflate_alignment(encode))
		return 1;

	/* flush */
	if (encode->flush) {
		if (DeflateOutputExist(encode->outputc)) {
			encode->output_call = 1;
			return 1;
		}
	}

	/* next */
	encode->state = Deflate_final;
	return 0;
}


/*
 *  Interface
 */
static int deflate_final_call(struct deflate *decode)
{
	DeflateState("final");
	return 0;
}

static int deflate_error_call(struct deflate *decode)
{
	DeflateState("error");
	return -1;
}

typedef int (*deflate_state_call)(struct deflate *);
static deflate_state_call deflate_state_array[] = {
	deflate_start,
	deflate_switch,
	deflate_plane,
	deflate_plane_loop,
	deflate_plane_header,
	deflate_plane_data,
	deflate_flush,
	deflate_final_call,
	deflate_error_call
};

int deflate_execute(struct deflate *encode)
{
	int index, check;
	deflate_state_call call;

#ifdef HYPD_DEBUG
	call = deflate_state_array[Deflate_error];
	DeflateDebug(call != deflate_error_call, "call error.");
#endif
	encode->input_call = 0;
	encode->output_call = 0;
	index = (int)encode->state;
	call = deflate_state_array[index];
	check = (*call)(encode);
	if (check < 0)
		encode->state = Deflate_error;

	return check;
}

int deflate_break(struct deflate *encode)
{
	if (encode->end_of_file) {
		DeflateError("already EOF.");
		return 1;
	}
	encode->input_call = 0;
	encode->end_of_file = 1;
	return 0;
}

int deflate_final(struct deflate *encode)
{
	return encode->state == Deflate_final;
}

int deflate_restart(struct deflate *encode)
{
	if (encode->state != Deflate_final) {
		DeflateError("state error.");
		return -1;
	}

	/* restart */
	encode->state = Deflate_start;
	return 0;
}

