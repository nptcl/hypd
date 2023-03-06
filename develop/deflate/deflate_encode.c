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
	encode->position += byte_size;
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
			encode->state = Deflate_fixed;
			return 0;

		case deflate_mode_dynamic_only:
			encode->state = Deflate_dynamic;
			return 0;

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
	int value, check;
	unsigned work_size;
	struct deflate_code *code;

#ifdef HYPD_DEBUG
	if (encode->state_index == 0) {
		DeflateState("plane_loop");
		encode->state_index = 1;
	}
#endif
	code = encode->code;
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
		code[work_size].code = (uint8_t)value;
		work_size++;
	}

	/* next */
	encode->work_size = work_size;
	return check;
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

static int deflate_plane_data(struct deflate *encode)
{
	uint8_t c;
	uint16_t x, y;
	int check;
	unsigned work_now, work_size;
	struct deflate_code *code;

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
	code = encode->code;
	work_now = encode->work_now;
	work_size = encode->work_size;
	while (work_now < work_size) {
		c = code[work_now].code;
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


/*
 *  Fixed
 */
static void deflate_fixed_loop(struct deflate_huffman *node,
		unsigned x, unsigned y, unsigned bit, unsigned plus)
{
	unsigned v;

	for (v = x; v <= y; v++) {
		node[v].size = bit;
		node[v].code = v + plus;
	}
}

static void deflate_fixed1(struct deflate_huffman *node)
{
	/*   0 - 143, 8bit,  00110000 ...  10111111, (+0x0030) */
	/* 144 - 255, 9bit, 110010000 ... 111111111, (+0x0190) */
	/* 256 - 279, 7bit,   0000000 ...   0010111, (+0x0000) */
	/* 280 - 285, 8bit,  11000000 ...  11000111, (+0x00C0) */
	deflate_fixed_loop(node, 0, 143, 8, 0x0030U);
	deflate_fixed_loop(node, 144, 255, 9, 0x0190U);
	deflate_fixed_loop(node, 256, 279, 7, 0x0000U);
	deflate_fixed_loop(node, 280, 285, 8, 0x00C0U);
}

static void deflate_fixed2(struct deflate_huffman *node)
{
	/*   0 - 29, 5bit,  00000 ...  11111, (+0) */
	deflate_fixed_loop(node, 0, 29, 5, 0);
}

static int deflate_fixed(struct deflate *encode)
{
	DeflateState("fixed");
	deflate_fixed1(encode->node1);
	deflate_fixed2(encode->node2);

	/* next */
	encode->state = Deflate_fixed_header;
	return 0;
}

static int deflate_fixed_header(struct deflate *encode)
{
	int eof;

	DeflateState("fixed_header");

	/* input */
	if (! deflate_write_p(encode, 3)) {
		encode->output_call = 1;
		return 1;
	}

	/* finalbit */
	eof = encode->end_of_file;
	deflate_write_1_unsafe(encode, eof);

	/* fixed */
	deflate_write_1_unsafe(encode, 0);
	deflate_write_1_unsafe(encode, 1);

	/* next */
	encode->state_index = 0;
	encode->input1 = 0;
	encode->input2 = 0;
	encode->state = Deflate_fixed_encode;
	return 0;
}

static int deflate_fixed_for(struct deflate *encode, int *ret)
{
	/* TODO */
	return -1;
}

static int deflate_fixed_encode(struct deflate *encode)
{
	int check, next;

	check = deflate_fixed_for(encode, &next);
	if (check < 0) {
		DeflateError("deflate_fixed_for error.");
		return -1;
	}
	if (check)
		return 1;

	/* next */
	encode->state = next?
		Deflate_fixed_header:
		Deflate_flush;
	return 0;
}


/*
 *  Dynamic
 */
static int deflate_dynamic(struct deflate *encode)
{
	encode->work_size = 0;

	/* next */
#ifdef HYPD_DEBUG
	encode->state_index = 0;
#endif
	encode->state = Deflate_dynamic_loop;
	return 0;
}

static unsigned deflate_hash_index(uint8_t a[])
{
	return (a[0] | (a[1] << 8) | (a[2] << 16)) % DEFLATE_HASH;
}

static void deflate_dynamic_distance(struct deflate *encode,
		struct deflate_hash *hash, unsigned *ret)
{
	*ret = (unsigned)(encode->position - hash->position);
}

static void deflate_dynamic_length(struct deflate *encode,
		struct deflate_hash *hash, unsigned *ret)
{
	*ret = 0;  /* TODO */
}

static void deflate_dynamic_lz77(struct deflate *encode, struct deflate_hash *hash)
{
	unsigned len, dist;
	struct deflate_code *code;

	deflate_dynamic_distance(encode, hash, &dist);
	deflate_dynamic_length(encode, hash, &len);
	hash->position = encode->position;

	/* code */
	code = encode->code + encode->work_size;
	code->code_p = 0;
	code->code = len;
	code->distance = dist;
	encode->work_size++;
}

static void deflate_dynamic_let3(struct deflate *encode, struct deflate_hash *hash)
{
	struct deflate_code *code;

	/* hash */
	hash->position = encode->position;

	/* code */
	code = encode->code + encode->work_size;
	code->code_p = 1;
	code->code = hash->key[0];
	encode->work_size++;
}

static void deflate_dynamic_let2(struct deflate *encode,
		uint8_t *array,
		struct deflate_hash *hash)
{
	hash->enable = 1;
	hash->key[0] = array[0];
	hash->key[1] = array[1];
	hash->key[2] = array[2];
	deflate_dynamic_let3(encode, hash);
}

static void deflate_dynamic_let1(struct deflate *encode, uint8_t *array)
{
	unsigned index;
	struct deflate_hash *hash;

	index = deflate_hash_index(array);
	hash = encode->hash + index;
	deflate_dynamic_let2(encode, array, hash);
}

static void deflate_dynamic_array(struct deflate *encode, uint8_t *array)
{
	int check;
	unsigned index;
	struct deflate_hash *hash;

	if (encode->work_size < 3) {
		deflate_dynamic_let1(encode, array);
		return;
	}

	/* hash */
	index = deflate_hash_index(array);
	hash = encode->hash + index;

	/* key check */
	check = hash->enable != 0
		&& hash->key[0] == array[0]
		&& hash->key[1] == array[1]
		&& hash->key[2] == array[2];
	if (! check) {
		deflate_dynamic_let2(encode, array, hash);
		return;
	}

	/* distance check */
	check = (encode->position - hash->position) < DEFLATE_LZ77;
	if (! check) {
		deflate_dynamic_let3(encode, hash);
		return;
	}

	/* lz77 */
	deflate_dynamic_lz77(encode, hash);
}

static void deflate_dynamic_code(struct deflate *encode, uint8_t x)
{
	uint8_t array[3];

	array[0] = x;
	array[1] = encode->input1;
	array[2] = encode->input2;
	deflate_dynamic_array(encode, array);
	encode->input1 = x;
	encode->input2 = array[1];
}

static int deflate_dynamic_loop(struct deflate *encode)
{
	int value;

#ifdef HYPD_DEBUG
	if (encode->state_index == 0) {
		DeflateState("dynamic_loop");
		encode->state_index = 1;
	}
#endif
	for (;;) {
		/* full buffer */
		if (DEFLATE_CODE <= encode->work_size) {
			encode->state = Deflate_dynamic_header;
			break;
		}

		/* input call */
		if (deflate_read(encode, &value)) {
			encode->input_call = 1;
			return 1;
		}

		/* end-of-file */
		if (value < 0)
			break;

		/* copy */
		deflate_dynamic_code(encode, (uint8_t)value);
	}

	/* next */
	encode->state = encode->work_size?
		Deflate_dynamic_header:
		Deflate_flush;
	return 0;
}

static int deflate_dynamic_header(struct deflate *encode)
{
	int eof;

	DeflateState("dynamic_header");

	/* input */
	if (! deflate_write_p(encode, 3)) {
		encode->output_call = 1;
		return 1;
	}

	/* finalbit */
	eof = encode->end_of_file;
	deflate_write_1_unsafe(encode, eof);

	/* dynamic */
	deflate_write_1_unsafe(encode, 1);
	deflate_write_1_unsafe(encode, 0);

	/* next */
	encode->state_index = 0;
	encode->input1 = 0;
	encode->input2 = 0;
	encode->state = Deflate_dynamic_encode;
	return 0;
}

static int deflate_dynamic_encode(struct deflate *encode)
{
	/* TODO */
	return -1;
}


/*
 *  flush
 */
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
static int deflate_final(struct deflate *decode)
{
	DeflateState("final");
	return 0;
}

static int deflate_failed(struct deflate *decode)
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
	deflate_fixed,
	deflate_fixed_header,
	deflate_fixed_encode,
	deflate_dynamic,
	deflate_dynamic_loop,
	deflate_dynamic_header,
	deflate_dynamic_encode,
	deflate_flush,
	deflate_final,
	deflate_failed
};

int deflate_execute(struct deflate *encode)
{
	int check;
	deflate_state_call call;

#ifdef HYPD_DEBUG
	call = deflate_state_array[Deflate_failed];
	DeflateDebug(call != deflate_failed, "call error.");
#endif
	do {
		encode->input_call = 0;
		encode->output_call = 0;
		call = deflate_state_array[encode->state];
		check = (*call)(encode);
		if (check < 0) {
			encode->state = Deflate_failed;
			return -1;
		}
		if (check)
			return 1;
	}
	while (encode->state != Deflate_final);

	return 0;
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

