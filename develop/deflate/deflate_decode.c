#include "deflate_decode.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 *  Error
 */
void inflate_error(const char *x)
{
	fprintf(stderr, "[ERROR] %s\n", x); \
}
#define InflateError(x) inflate_error(x)
#define InflateError1(x,y) { \
	char __message[256]; \
	snprintf(__message, 256, x, y); \
	InflateError(__message); \
}
#ifdef HYPD_DEBUG
#define InflateDebug(x, y) { \
	if (x) { \
		InflateError(y); \
		exit(1); \
	} \
}
#else
#define InflateDebug(x, y)
#endif


/*
 *  Info
 */
#ifdef HYPD_INFO
#define INFLATE_INFO
#endif

#ifdef INFLATE_INFO
#include <ctype.h>
#include <stdarg.h>

#define InflateState(x) { \
	fprintf(stderr, "Inflate.state: %s\n", (x)); \
	fflush(stderr); \
}
void inflate_info(const char *str, ...)
{
	va_list list;

	va_start(list, str);
	fprintf(stderr, "Inflate.");
	vfprintf(stderr, str, list);
	va_end(list);
	fflush(NULL);
}

/* #define InflateInfo(...) inflate_info(__VA_ARGS__) */
#define InflateInfo1(x) inflate_info(x)
#define InflateInfo2(x,a) inflate_info(x,a)
#define InflateInfo3(x,a,b) inflate_info(x,a,b)
#define InflateInfo4(x,a,b,c) inflate_info(x,a,b,c)
#define InflateInfo5(x,a,b,c,d) inflate_info(x,a,b,c,d)
#define InflateInfo6(x,a,b,c,d,e) inflate_info(x,a,b,c,d,e)
#else
#define InflateState(x)
/* #define InflateInfo(x) */
#define InflateInfo1(x)
#define InflateInfo2(x,a)
#define InflateInfo3(x,a,b)
#define InflateInfo4(x,a,b,c)
#define InflateInfo5(x,a,b,c,d)
#define InflateInfo6(x,a,b,c,d,e)
#endif

#define InflateOutputEmpty(x)	((x) < INFLATE_RING2)
#define InflateOutputExist(x)	((x) != 0)
#define InflateOutputFull(x)	(INFLATE_RING2 <= (x))

void inflate_init(struct inflate *decode)
{
#ifdef HYPD_DEBUG
	memset(decode, 0xAA, sizeof(struct inflate));
#endif
	decode->input_call = 0;
	decode->output_call = 0;
	decode->flush = 0;
	decode->inputa = 0;
	decode->inputb = 0;
	decode->inputc = 0;
	decode->inputx = 0;
	decode->outputa = 0;
	decode->outputb = 0;
	decode->outputc = 0;
	decode->output_callback = NULL;
	decode->output_instance = NULL;

	/* next */
	decode->state = Inflate_start;
}


/*
 *  Ring
 */
const void *inflate_input(struct inflate *decode,
		const void *ptr, size_t size, size_t *ret)
{
	char *output;
	const char *input;
	size_t x, y, z, b, c;

	/* zero */
	if (size == 0) {
		*ret = 0;
		return ptr;
	}

	/* full */
	c = decode->inputc;
	if (INFLATE_RING1 <= c) {
		*ret = 0;
		return ptr;
	}

	/* size */
	x = INFLATE_RING1 - c;
	if (size < x)
		x = size;

	/* body */
	input = (const char *)ptr;
	output = (char *)(decode->input);
	b = decode->inputb;
	z = b + x;
	if (z < INFLATE_RING1) {
		memcpy(output + b, input, x);
	}
	else {
		y = INFLATE_RING1 - b;
		z = x - y;
		memcpy(output + b, input, y);
		memcpy(output, input + y, z);
	}
	decode->inputb = z;
	input += x;

	/* result */
	decode->inputc += x;
	*ret = x;
	return (const void *)input;
}

void *inflate_output(struct inflate *decode,
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
	c = decode->outputc;
	if (c == 0) {
		*ret = 0;
		return ptr;
	}

	/* size */
	x = (size < c)? size: c;

	/* body */
	output = (char *)ptr;
	input = (const char *)(decode->output);
	a = decode->outputa;
	z = x + a;
	if (z < INFLATE_RING2) {
		memcpy(output, input + a, x);
	}
	else {
		y = INFLATE_RING2 - a;
		z = x - y;
		memcpy(output, input + a, y);
		memcpy(output + y, input, z);
	}
	decode->outputa = z;
	output += x;

	/* result */
	decode->outputc -= x;
	*ret = x;
	return (void *)output;
}

static void inflate_inputa_forward(struct inflate *decode, unsigned byte_size)
{
	InflateDebug(decode->inputc < byte_size, "inputc error.");
	decode->inputc -= byte_size;
	decode->inputa += byte_size;
	if (INFLATE_RING1 <= decode->inputa)
		decode->inputa = decode->inputa % INFLATE_RING1;
}

static void inflate_outputb_forward(struct inflate *decode, unsigned byte_size)
{
	InflateDebug(INFLATE_RING2 < (decode->outputc + byte_size), "outputc error.");
	decode->outputc += byte_size;
	decode->outputb += byte_size;
	if (INFLATE_RING2 <= decode->outputb)
		decode->outputb = decode->outputb % INFLATE_RING2;
}


/*
 *  Read
 */
static int inflate_read_p(struct inflate *decode, unsigned bit_size)
{
	unsigned c, x;

	if (bit_size == 0)
		return 1;
	c = decode->inputc;
	if (c == 0)
		return 0;
	x = c * 8U - decode->inputx;
	return bit_size <= x;
}

static void inflate_move(struct inflate *decode, unsigned bit_size)
{
	unsigned x, y;

	InflateDebug(! inflate_read_p(decode, bit_size), "inflate_move error.");
	bit_size += decode->inputx;
	x = bit_size / 8U;
	y = bit_size % 8U;
	if (x)
		inflate_inputa_forward(decode, x);
	decode->inputx = y;
}

static int inflate_read_1(struct inflate *decode, unsigned bit, int *ret)
{
	unsigned c, x, y;

	c = decode->inputc;
	bit += decode->inputx;
	x = bit / 8U;
	if (c <= x) {
		*ret = 0;
		return 1;
	}
	y = bit % 8U;

	/* read */
	x += decode->inputa;
	if (INFLATE_RING1 <= x)
		x = x % INFLATE_RING1;
	x = decode->input[x];
	*ret = (x >> y) & 0x01;
	return 0;
}

static int inflate_peek(struct inflate *decode,
		unsigned shift, unsigned bit, unsigned *ret)
{
	uint8_t *input, v;
	unsigned inputx, inputa, x, y, z, value, i;

	if (bit == 0) {
		*ret = 0;
		return 0;
	}
	InflateDebug(32U < bit, "bit size error2.");
	inputx = decode->inputx;
	z = inputx + shift;
	x = z + bit;
	y = decode->inputc * 8U;
	if (y < x) {
		*ret = 0;
		return 1;
	}

	input = decode->input;
	inputa = decode->inputa;
	x = z / 8U;
	y = z % 8U;
	inputa += x;
	if (INFLATE_RING1 <= inputa)
		inputa = inputa % INFLATE_RING1;
	v = input[inputa];
	value = 0;
	for (i = 0; i < bit; i++) {
		z = (v >> y) & 0x01;
		if (z)
			value |= (1U << i);
		y++;
		if (8U <= y) {
			y = 0;
			inputa++;
			if (INFLATE_RING1 <= inputa)
				inputa = 0;
			v = input[inputa];
		}
	}
	*ret = value;
	return 0;
}

static int inflate_read(struct inflate *decode, unsigned bit, unsigned *ret)
{
	int check;

	check = inflate_peek(decode, 0, bit, ret);
	if (check)
		return check;
	inflate_move(decode, bit);

	return 0;
}

int inflate_alignment(struct inflate *decode)
{
	unsigned bit;

	bit = decode->inputx;
	if (bit == 0)
		return 0;
	inflate_move(decode, 8 - bit);

	return 0;
}

int inflate_byte_read_p(struct inflate *decode, unsigned size)
{
	if (decode->inputx)
		return -1;
	if (decode->inputc < size)
		return 1;

	return 0; /* readable */
}

int inflate_byte_getc(struct inflate *decode, uint8_t *ret)
{
	if (decode->inputx)
		return -1;
	if (decode->inputc == 0)
		return 1;
	*ret = decode->input[decode->inputa];
	inflate_inputa_forward(decode, 1);

	return 0;
}


/*
 *  Start
 */
static int inflate_start(struct inflate *decode)
{
	InflateState("start");
	decode->finalbit = 0;
	decode->lz77now = 0;
	decode->lz77size = 0;

	/* next */
	decode->state = Inflate_header;
	return 0;
}


/*
 *  Header
 */
static int inflate_header(struct inflate *decode)
{
	unsigned x, v;

	InflateState("header");

	/* finalbit */
	if (decode->finalbit) {
		decode->state = Inflate_flush;
		return 0;
	}

	/* input */
	if (! inflate_read_p(decode, 3)) {
		decode->input_call = 1;
		return 1;
	}

	/* value */
	inflate_peek(decode, 0, 1, &x);
	inflate_peek(decode, 1, 2, &v);
	decode->finalbit = (x != 0);
	switch (v) {
		case 0: /* Plane */
			decode->state = Inflate_plane;
			break;

		case 1: /* Fixed */
			decode->state = Inflate_fixed;
			break;

		case 2: /* Dynamic */
			decode->state = Inflate_dynamic;
			break;

		default: /* error */
			decode->state = Inflate_failed;
			return -1;
	}

	/* next */
	inflate_move(decode, 3);
	return 0;
}


/*
 *  Plane
 */
static int inflate_plane(struct inflate *decode)
{
	uint16_t a, b;
	unsigned x, y;

	InflateState("plane");
	if (inflate_alignment(decode)) {
		InflateError("inflate_alignment error.");
		return -1;
	}
	x = 8U * (2U + 2U); /* len, nlen */
	if (! inflate_read_p(decode, x)) {
		decode->input_call = 1;
		return 1;
	}

	/* size */
	inflate_read(decode, 16, &x);
	inflate_read(decode, 16, &y);
	a = (uint16_t)x;
	b = (uint16_t)(~y);
	if (a != b) {
		InflateError("plane size error.");
		return -1;
	}
	decode->plane_size = x;

	/* next */
#ifdef HYPD_DEBUG
	decode->state_index = 0;
#endif
	decode->state = Inflate_plane_pipe;
	return 0;
}

static void inflate_input_recv(
		struct inflate *decode, uint8_t *data, unsigned size)
{
	unsigned a, z;

	InflateDebug(decode->inputc < size, "inputc error.");
	InflateDebug(INFLATE_RING1 < size, "size error.");
	a = decode->inputa;
	z = a + size;
	if (z < INFLATE_RING1) {
		memcpy(data, decode->input + a, size);
	}
	else {
		z = INFLATE_RING1 - a;
		memcpy(data, decode->input + a, z);
		memcpy(data + z, decode->input, size - z);
	}
}

static void inflate_output_lz77(struct inflate *decode,
		const uint8_t *ptr, unsigned size)
{
	unsigned a, z;
	uint8_t *lz77;

	/* large size */
	lz77 = decode->lz77;
	if (INFLATE_LZ77 <= size) {
		z = size - INFLATE_LZ77;
		memcpy(lz77, ptr + z, INFLATE_LZ77);
		decode->lz77now = 0;
		decode->lz77size = INFLATE_LZ77;
		return;
	}

	/* copy only */
	a = decode->lz77now;
	z = a + size;
	if (z < INFLATE_LZ77) {
		memcpy(lz77 + a, ptr, size);
		decode->lz77now += size;
	}
	else {
		z = INFLATE_LZ77 - a;
		memcpy(lz77 + a, ptr, z);
		memcpy(lz77, ptr + z, size - z);
		decode->lz77now += size;
		if (INFLATE_LZ77 <= decode->lz77now)
			decode->lz77now = decode->lz77now % INFLATE_LZ77;
	}

	/* size */
	if (decode->lz77size < INFLATE_LZ77) {
		decode->lz77size += size;
		if (INFLATE_LZ77 < decode->lz77size)
			decode->lz77size = INFLATE_LZ77;
	}
}

static void inflate_output_memcpy(struct inflate *decode,
		uint8_t *dst, const uint8_t *src, unsigned size)
{
	unsigned i;
	inflate_callback call;
	void *inst;

	/* copy */
	memcpy(dst, src, size);

	/* callback */
	call = decode->output_callback;
	if (call) {
		inst = decode->output_instance;
		for (i = 0; i < size; i++)
			(*call)(inst, src[i]);
	}
}

static void inflate_output_send(
		struct inflate *decode, const uint8_t *data, unsigned size)
{
	unsigned b, z;

	InflateDebug((INFLATE_RING2 - decode->outputc) < size, "outputc error.");
	InflateDebug(INFLATE_RING2 < size, "size error.");
	b = decode->outputb;
	z = b + size;
	if (z < INFLATE_RING2) {
		inflate_output_memcpy(decode, decode->output + b, data, size);
	}
	else {
		z = INFLATE_RING2 - b;
		inflate_output_memcpy(decode, decode->output + b, data, z);
		inflate_output_memcpy(decode, decode->output, data + z, size - z);
	}
}

static void inflate_plane_copy(struct inflate *decode, unsigned size)
{
	uint8_t data[INFLATE_RING2];  /* RING2 */

	InflateDebug(INFLATE_RING2 < size, "size error.");
	inflate_input_recv(decode, data, size);
	inflate_output_send(decode, data, size);
	inflate_inputa_forward(decode, size);
	inflate_outputb_forward(decode, size);
	inflate_output_lz77(decode, data, size);
}

static int inflate_plane_pipe(struct inflate *decode)
{
	unsigned size, psize, inputc, output_space;

#ifdef HYPD_DEBUG
	if (decode->state_index == 0) {
		InflateState("plane_pipe");
		decode->state_index = 1;
	}
#endif

	for (psize = decode->plane_size; psize; psize -= size) {
		/* input */
		inputc = decode->inputc;
		if (inputc == 0) {
			decode->input_call = 1;
			goto interrupt;
		}

		/* output */
		output_space = INFLATE_RING2 - decode->outputc;
		if (output_space == 0) {
			decode->output_call = 1;
			goto interrupt;
		}

		/* copy size */
		if (psize < inputc)
			size = psize;
		else
			size = inputc;
		if (output_space < size)
			size = output_space;
		if (INFLATE_RING2 < size)
			size = INFLATE_RING2;  /* RING2 */

		/* pipe */
		inflate_plane_copy(decode, size);
	}

	/* next */
	decode->plane_size = 0;
	decode->state = Inflate_header;
	return 0;

interrupt:
	decode->plane_size = psize;
	return 1;
}


/*
 *  Huffman
 */
static unsigned inflate_huffman_new(struct inflate_huffman *huffman)
{
	unsigned now;
	struct inflate_node *root;

	now = huffman->now;
	InflateDebug(huffman->size <= now, "cursor error");
	huffman->now++;
	root = huffman->node + now;
	root->link0 = 0;
	root->link1 = 0;
	root->term0 = 0;
	root->term1 = 0;
	root->value0 = 0;
	root->value1 = 0;

	return now;
}

static void inflate_huffman_init(struct inflate *decode)
{
	struct inflate_huffman *huffman1;
	struct inflate_huffman *huffman2;

	huffman1 = &(decode->huffman1);
	huffman1->now = 0;
	huffman1->size = INFLATE_HUFFMAN_SIZE1;
	huffman1->node = decode->node1;
	huffman1->root = inflate_huffman_new(huffman1);
	huffman2 = &(decode->huffman2);
	huffman2->now = 0;
	huffman2->size = INFLATE_HUFFMAN_SIZE2;
	huffman2->node = decode->node2;
	huffman2->root = inflate_huffman_new(huffman2);
}

static int inflate_huffman_bit0(struct inflate_huffman *huffman, unsigned *pos)
{
	struct inflate_node *root;

	root = huffman->node + *pos;
	if (root->link0) {
		*pos = root->value0;
		return 0;
	}
	else if (root->term0) {
		InflateError("term0 error.");
		return -1;
	}
	else {
		*pos = inflate_huffman_new(huffman);
		root->value0 = *pos;
		root->link0 = 1;
		return 0;
	}
}

static int inflate_huffman_bit1(struct inflate_huffman *huffman, unsigned *pos)
{
	struct inflate_node *root;

	root = huffman->node + *pos;
	if (root->link1) {
		*pos = root->value1;
		return 0;
	}
	else if (root->term1) {
		InflateError("term1 error.");
		return -1;
	}
	else {
		*pos = inflate_huffman_new(huffman);
		root->value1 = *pos;
		root->link1 = 1;
		return 0;
	}
}

static int inflate_huffman_add0(struct inflate_node *root, unsigned v)
{
	if (root->link0) {
		InflateError("link0 error.");
		return -1;
	}
	else if (root->term0) {
		InflateError("term0 error.");
		return -1;
	}
	else {
		root->value0 = v;
		root->term0 = 1;
		return 0;
	}
}

static int inflate_huffman_add1(struct inflate_node *root, unsigned v)
{
	if (root->link1) {
		InflateError("link1 error.");
		return -1;
	}
	else if (root->term1) {
		InflateError("term1 error.");
		return -1;
	}
	else {
		root->value1 = v;
		root->term1 = 1;
		return 0;
	}
}

#ifdef INFLATE_INFO
static void inflate_huffman_string(char *str, unsigned code, unsigned len)
{
	int check;
	unsigned i;

	for (i = 0; i < len; i++) {
		check = (code >> (len - i - 1)) & 0x01;
		str[i] = check? '1': '0';
	}
	str[i] = '\0';
}
#endif

static int inflate_huffman_add(struct inflate_huffman *huffman,
		unsigned x, unsigned bit, unsigned code)
{
#ifdef INFLATE_INFO
	char data[100];
#endif
	unsigned bitm1, pos, i, check;
	struct inflate_node *root;

#ifdef INFLATE_INFO
	inflate_huffman_string(data, code, bit);
	InflateInfo5("huffman.add[0x%04X] = %s:%u, %u\n", x, data, bit, code);
#endif

	if (bit == 0)
		return 0;
	bitm1 = bit - 1;
	pos = huffman->root;

	/* link */
	for (i = 0; i < bitm1; i++) {
		check = (code >> (bit - i - 1)) & 0x01;
		if (check == 0)
			check = inflate_huffman_bit0(huffman, &pos);
		else
			check = inflate_huffman_bit1(huffman, &pos);
		if (check)
			return check;
	}

	/* term */
	root = huffman->node + pos;
	check = (code & 0x01);
	if (check == 0)
		return inflate_huffman_add0(root, x);
	else
		return inflate_huffman_add1(root, x);
}

static int inflate_huffman1_get(struct inflate *decode,
		int *ret, unsigned *rsize)
{
	int check;
	unsigned i, pos;
	struct inflate_node *node, *root;
	struct inflate_huffman *huffman;

	huffman = &(decode->huffman1);
	node = huffman->node;
	pos = huffman->root;
	InflateDebug(pos < 0, "position error.");
	i = 0;
	for (;;) {
		if (inflate_read_1(decode, i, &check)) {
			/* read request */
			*ret = 0;
			*rsize = 0;
			return 1;
		}
		i++;
		root = node + pos;
		if (check) {
			pos = root->value1;
			if (root->link1)
				continue;
			if (root->term1)
				break;
		}
		else {
			pos = root->value0;
			if (root->link0)
				continue;
			if (root->term0)
				break;
		}

		/* not found */
		*ret = -1;
		*rsize = 0;
		return 0;
	}

	/* result */
	*ret = (int)pos;
	*rsize = i;
	return 0;
}

#define INFLATE_HUFFMAN_BIT		16
static void inflate_huffman_step1(
		unsigned *bl_count, uint8_t *count, unsigned count_size)
{
	unsigned x, y;

	/* clear */
	memset(bl_count, 0, sizeof(unsigned) * INFLATE_HUFFMAN_BIT);

	/* count */
	for (x = 0; x < count_size; x++) {
		y = count[x];
		bl_count[y]++;
	}
}

static void inflate_huffman_step2(
		unsigned *bl_count, unsigned *next_code, unsigned max_bits)
{
	unsigned code, x;

	code = 0;
	bl_count[0] = 0;
	for (x = 1; x <= max_bits; x++) {
#ifdef INFLATE_INFO
		if (bl_count[x]) {
			InflateInfo3("bl_count[%u] = %u\n", x, bl_count[x]);
		}
#endif
		code = (code + bl_count[x - 1]) << 1;
		next_code[x] = code;
	}
	next_code[0] = 0;
}

static int inflate_huffman_step3(
		struct inflate_huffman *huffman,
		unsigned *next_code, uint8_t *count,
		unsigned count_size, unsigned max_bits)
{
	int check;
	unsigned x, len, code;
#ifdef INFLATE_INFO
	unsigned y;
#endif

#ifdef INFLATE_INFO
	for (y = 1; y <= max_bits; y++) {
#endif
		for (x = 0; x < count_size; x++) {
			len = count[x];
#ifdef INFLATE_INFO
			check = (len == y);
#else
			check = (len != 0);
#endif
			if (check) {
				code = next_code[len];
				next_code[len]++;
				/* code, len */
				if (inflate_huffman_add(huffman, x, len, code)) {
					InflateError("inflate_huffman_add error.");
					return -1;
				}
			}
		}
#ifdef INFLATE_INFO
	}
#endif

	return 0;
}

static int inflate_huffman_make(
		struct inflate_huffman *huffman,
		uint8_t *count,
		unsigned count_size,
		unsigned max_bits)
{
	unsigned next_code[INFLATE_HUFFMAN_BIT];
	unsigned bl_count[INFLATE_HUFFMAN_BIT];

	InflateInfo3("huffman.make: %u, %u\n", count_size, max_bits);
	inflate_huffman_step1(bl_count, count, count_size);
	inflate_huffman_step2(bl_count, next_code, max_bits);
	return inflate_huffman_step3(huffman, next_code, count, count_size, max_bits);
}


/*
 *  Fixed
 */
static int inflate_fixed_loop(struct inflate_huffman *huffman,
		unsigned x, unsigned y, unsigned bit, unsigned plus)
{
	unsigned v;

	for (v = x; v <= y; v++) {
		if (inflate_huffman_add(huffman, v, bit, v + plus))
			return -1;
	}

	return 0;
}

static int inflate_fixed1(struct inflate_huffman *huffman)
{
	/*   0 - 143, 8bit,  00110000 ...  10111111, (+0x0030) */
	/* 144 - 255, 9bit, 110010000 ... 111111111, (+0x0190) */
	/* 256 - 279, 7bit,   0000000 ...   0010111, (+0x0000) */
	/* 280 - 285, 8bit,  11000000 ...  11000111, (+0x00C0) */
	if (inflate_fixed_loop(huffman, 0, 143, 8, 0x0030U))
		goto error;
	if (inflate_fixed_loop(huffman, 144, 255, 9, 0x0190U))
		goto error;
	if (inflate_fixed_loop(huffman, 256, 279, 7, 0x0000U))
		goto error;
	if (inflate_fixed_loop(huffman, 280, 285, 8, 0x00C0U))
		goto error;
	return 0;

error:
	InflateError("inflate_fixed_loop error.");
	return -1;
}

static int inflate_fixed2(struct inflate_huffman *huffman)
{
	/*   0 - 29, 5bit,  00000 ...  11111, (+0) */
	if (inflate_fixed_loop(huffman, 0, 29, 5, 0)) {
		InflateError("inflate_fixed_loop error.");
		return -1;
	}
	return 0;
}

static int inflate_fixed(struct inflate *decode)
{
	int check;

	InflateState("fixed");

	/* huffman1 */
	inflate_huffman_init(decode);
	check = inflate_fixed1(&(decode->huffman1));
	if (check)
		return check;

	/* huffman2 */
	check = inflate_fixed2(&(decode->huffman2));
	if (check)
		return check;

	/* decode */
	decode->state_index = 0;
	decode->state = Inflate_decode;
	return 0;
}


/*
 *  Dynamic
 */
static int inflate_dynamic_make(struct inflate *decode)
{
	int check;
	unsigned size;
	uint8_t *count;
	struct inflate_huffman *huffman;

	InflateInfo1("inflate_dynamic_make\n");
	/* hlit */
	inflate_huffman_init(decode);
	huffman = &(decode->huffman1);
	count = decode->count;
	size = decode->dynamic_hlit;
	check = inflate_huffman_make(huffman, count, size, 15);
	if (check) {
		InflateError("inflate_huffman_make huffman1 (hlit) error.");
		return -1;
	}

	/* hdist */
	huffman = &(decode->huffman2);
	count = decode->count + decode->dynamic_hlit;
	size = decode->dynamic_hdist;
	check = inflate_huffman_make(huffman, count, size, 15);
	if (check) {
		InflateError("inflate_huffman_make huffman2 (hdist) error.");
		return -1;
	}

	/* next */
	decode->state_index = 0;
	decode->state = Inflate_decode;
	return 0;
}

static int inflate_dynamic_code(struct inflate *decode, int value, unsigned length)
{
	unsigned index;

	index = decode->state_index;
	decode->count[index] = value;
	decode->state_index++;
	if (decode->dynamic_size < decode->state_index) {
		InflateError("dynamic_size error.");
		return -1;
	}
	inflate_move(decode, length);

	return 0;
}

static int inflate_dynamic_times(struct inflate *decode, unsigned length)
{
	uint8_t prev, *count;
	unsigned index, size, x;

	/* previous value */
	index = decode->state_index;
	if (index == 0) {
		InflateError("root error.");
		return -1;
	}
	count = decode->count;
	prev = count[index - 1];

	/* times */
	if (inflate_peek(decode, length, 2, &size))
		return 1;
	size += 3;
	InflateInfo2("dynamic_times: %u\n", size);

	/* set */
	for (x = 0; x < size; x++)
		count[index + x] = prev;

	/* forward */
	decode->state_index += size;
	if (decode->dynamic_size < decode->state_index) {
		InflateError("dynamic_size error.");
		return -1;
	}
	inflate_move(decode, length + 2);

	return 0;
}

static int inflate_dynamic_zero3(struct inflate *decode, unsigned length)
{
	unsigned size;

	if (inflate_peek(decode, length, 3, &size))
		return 1;
	size += 3;
	InflateInfo2("dynamic_zero3: %u\n", size);

	decode->state_index += size;
	if (decode->dynamic_size < decode->state_index) {
		InflateError("dynamic_size error.");
		return -1;
	}
	inflate_move(decode, length + 3);

	return 0;
}

static int inflate_dynamic_zero7(struct inflate *decode, unsigned length)
{
	unsigned size;

	if (inflate_peek(decode, length, 7, &size))
		return 1;
	size += 11;
	InflateInfo2("dynamic_zero7: %u\n", size);

	decode->state_index += size;
	if (decode->dynamic_size < decode->state_index) {
		InflateInfo3("dynamic_size error: %u, %u\n",
				decode->dynamic_size,
				decode->state_index);
		InflateError("dynamic_size error.");
		return -1;
	}
	inflate_move(decode, length + 7);

	return 0;
}

static int inflate_dynamic_add(struct inflate *decode, int value, unsigned length)
{
#ifdef INFLATE_INFO
	int c;

	c = decode->state_index;
	InflateInfo6("dynamic_value: 0x%X/0x%X (%c), %u, %u\n",
			c, decode->dynamic_size, isgraph(c)? c: ' ', value, length);
#endif
	if (0 <= value && value <= 15)
		return inflate_dynamic_code(decode, value, length);
	if (value == 16)
		return inflate_dynamic_times(decode, length);
	if (value == 17)
		return inflate_dynamic_zero3(decode, length);
	if (value == 18)
		return inflate_dynamic_zero7(decode, length);

	/* error */
	InflateError("inflate_dynamic_add error.");
	return -1;
}

static int inflate_dynamic_value(struct inflate *decode)
{
	int check, value;
	unsigned length;

	InflateState("dynamic_value");
	while (decode->state_index < decode->dynamic_size) {
		/* get */
		check = inflate_huffman1_get(decode, &value, &length);
		if (check) {
			decode->input_call = 1;
			return 1;
		}
		if (value < 0) {
			InflateError("huffman code error.");
			return -1;
		}

		/* add */
		check = inflate_dynamic_add(decode, value, length);
		if (check < 0) {
			InflateError1("Invalid value, %d.", value);
			return -1;
		}
		if (check) {
			decode->input_call = 1;
			return 1;
		}
	}

	return inflate_dynamic_make(decode);
}

static int inflate_dynamic_tree(struct inflate *decode)
{
	int check;
	uint8_t *count;
	struct inflate_huffman *huffman;

	inflate_huffman_init(decode);
	huffman = &(decode->huffman1);
	count = decode->count;
	check = inflate_huffman_make(huffman, count, 19, 7);
	if (check) {
		InflateError("inflate_huffman_make error.");
		return -1;
	}
	memset(count, 0, INFLATE_HUFFMAN_CODE);

	return 0;
}

static const int inflate_dynamic_table[] = {
	16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15, -1
};

static int inflate_dynamic_length(struct inflate *decode)
{
	unsigned x, y, hclen, value;

	InflateState("dynamic_length");

	/* hclen */
	hclen = decode->dynamic_hclen;
	for (x = decode->state_index; x < hclen; x++) {
		if (inflate_read(decode, 3, &value)) {
			decode->state_index = x;
			decode->input_call = 1;
			return 1;
		}
		y = inflate_dynamic_table[x];
		InflateDebug(y < 0, "table error.");
		decode->count[y] = value;
	}

#ifdef INFLATE_INFO
	for (y = 0; y < 19; y++) {
		if (decode->count[y]) {
			InflateInfo3("hclen[%u] = %u\n", y, decode->count[y]);
		}
	}
#endif

	/* huffman */
	if (inflate_dynamic_tree(decode)) {
		InflateError("inflate_dynamic_tree error.");
		return -1;
	}

	/* next */
	decode->state_index = 0;
	decode->state = Inflate_dynamic_value;
	return inflate_dynamic_value(decode);
}

static int inflate_dynamic_header(struct inflate *decode)
{
	unsigned label, value;

	InflateState("dynamic_header");
	label = decode->state_index;
	if (label == 0)
		goto hlit;
	if (label == 1)
		goto hdist;
	if (label == 2)
		goto hclen;
	/* error */
	return -1;

hlit:
	/* HLIT: 5 [bit] */
	if (inflate_read(decode, 5, &value)) {
		decode->state_index = 0;
		decode->input_call = 1;
		return 1;
	}
	decode->dynamic_hlit = value + 257U;
	InflateInfo3("hlit: %u [%u]\n", value, decode->dynamic_hlit);

hdist:
	/* HDIST: 5 [bit] */
	if (inflate_read(decode, 5, &value)) {
		decode->state_index = 1;
		decode->input_call = 1;
		return 1;
	}
	decode->dynamic_hdist = value + 1U;
	decode->dynamic_size = decode->dynamic_hlit + decode->dynamic_hdist;
	InflateInfo3("hdist: %u [%u]\n", value, decode->dynamic_hdist);
	InflateInfo2("dynamic_size: %u\n", decode->dynamic_size);

hclen:
	/* HCLEN: 4 [bit] */
	if (inflate_read(decode, 4, &value)) {
		decode->state_index = 2;
		decode->input_call = 1;
		return 1;
	}
	decode->dynamic_hclen = value + 4U;
	InflateInfo3("hclen: %u [%u]\n", value, decode->dynamic_hclen);

	/* next */
	decode->state_index = 0;
	decode->state = Inflate_dynamic_length;
	memset(decode->count, 0, INFLATE_HUFFMAN_CODE); /* 1+2 */
	return inflate_dynamic_length(decode);
}

static int inflate_dynamic(struct inflate *decode)
{
	InflateState("dynamic");
	decode->state = Inflate_dynamic_header;
	decode->state_index = 0;
	decode->dynamic_hlit = 0;
	decode->dynamic_hdist = 0;
	decode->dynamic_hclen = 0;
	decode->dynamic_size = 0;
	return inflate_dynamic_header(decode);
}


/*
 *  Decode
 */
static const unsigned inflate_lz77_array1[29][2] = {
	{ 0,   3 }, { 0,   4 }, { 0,   5 }, { 0,   6 },  /* 257 */
	{ 0,   7 }, { 0,   8 }, { 0,   9 }, { 0,  10 },  /* 261 */
	{ 1,  11 }, { 1,  13 }, { 1,  15 }, { 1,  17 },  /* 265 */
	{ 2,  19 }, { 2,  23 }, { 2,  27 }, { 2,  31 },  /* 269 */
	{ 3,  35 }, { 3,  43 }, { 3,  51 }, { 3,  59 },  /* 273 */
	{ 4,  67 }, { 4,  83 }, { 4,  99 }, { 4, 115 },  /* 277 */
	{ 5, 131 }, { 5, 163 }, { 5, 195 }, { 5, 227 },  /* 281 */
	{ 0, 258 }                                       /* 285 */
};

static int inflate_lz77_extend1(struct inflate *decode,
		unsigned bias, unsigned value, unsigned *rvalue, unsigned *rsize)
{
	unsigned x, y, extend;

	/* array */
	if (285 < value) {
		InflateError("value error.");
		return -1;
	}
	value -= 257;
	x = inflate_lz77_array1[value][0];
	y = inflate_lz77_array1[value][1];

	/* read */
	if (inflate_peek(decode, bias, x, &extend))
		return 1;
	*rvalue = extend + y;
	*rsize = x;

	return 0;
}

static const unsigned inflate_lz77_array2[30][2] = {
	{  0,     1 }, {  0,     2 }, {  0,     3 }, {  0,     4 },  /* 0 */
	{  1,     5 }, {  1,     7 }, {  2,     9 }, {  2,    13 },  /* 4 */
	{  3,    17 }, {  3,    25 }, {  4,    33 }, {  4,    49 },  /* 8 */
	{  5,    65 }, {  5,    97 }, {  6,   129 }, {  6,   193 },  /* 12 */
	{  7,   257 }, {  7,   385 }, {  8,   513 }, {  8,   769 },  /* 16 */
	{  9,  1025 }, {  9,  1537 }, { 10,  2049 }, { 10,  3073 },  /* 20 */
	{ 11,  4097 }, { 11,  6145 }, { 12,  8193 }, { 12, 12289 },  /* 24 */
	{ 13, 16385 }, { 13, 24577 }                                 /* 28 */
};

static int inflate_lz77_extend2(struct inflate *decode,
		unsigned bias, unsigned value, unsigned *rvalue, unsigned *rsize)
{
	unsigned x, y, extend;

	/* array */
	if (29 < value) {
		InflateError("value error.");
		return -1;
	}
	x = inflate_lz77_array2[value][0];
	y = inflate_lz77_array2[value][1];

	/* read */
	if (inflate_peek(decode, bias, x, &extend)) {
		decode->input_call = 1;
		return 1;
	}
	*rvalue = extend + y;
	*rsize = x;

	return 0;
}

static int inflate_decode_loop(struct inflate *decode)
{
	int check;
	uint8_t v, *lz77, *output;
	unsigned lz77now, lz77size;
	unsigned loop_size, loop_now, loop_pos;
	unsigned i, size, output_space, x, y, z;
	inflate_callback callback;
	void *instance;

	loop_size = decode->loop_size;
	loop_pos = decode->loop_pos;
	loop_now = decode->loop_now;
	lz77 = decode->lz77;
	lz77now = decode->lz77now;
	lz77size = decode->lz77size;
	output = decode->output;
	output_space = INFLATE_RING2 - decode->outputc;
	callback = decode->output_callback;
	instance = decode->output_instance;

	InflateDebug(loop_size < loop_now, "now error.");
	InflateDebug(loop_pos == 0, "position error.");
	if (lz77size <= loop_pos) {
		InflateError("position error.");
		return -1;
	}
	size = loop_size - loop_now;
	if (output_space < size) {
		size = output_space;
		check = 1;
	}
	else {
		check = 0;
	}
	x = decode->outputb;
	y = lz77now;
	if (lz77now < loop_pos)
		z = lz77now + INFLATE_LZ77 - loop_pos;
	else
		z = lz77now - loop_pos;

	for (i = 0; i < size; i++) {
		v = output[x++] = lz77[y++] = lz77[z++];
		if (INFLATE_RING2 <= x)
			x = 0;
		if (INFLATE_LZ77 <= y)
			y = 0;
		if (INFLATE_LZ77 <= z)
			z = 0;
		if (callback)
			(*callback)(instance, v);
	}

	inflate_outputb_forward(decode, size);
	lz77now += size;
	lz77size += size;
	decode->lz77now = lz77now % INFLATE_LZ77;
	decode->lz77size = (lz77size < INFLATE_LZ77)? lz77size: INFLATE_LZ77;
	decode->loop_now += size;

	return check;
}

#define Inflate_decode_tree(check, str, pos, value, label_1, label_2) { \
	if (check) { \
		if (str->link1) { \
			pos = str->value1; \
			goto label_1; \
		} \
		if (str->term1) { \
			value = str->value1; \
			goto label_2; \
		} \
	} \
	else { \
		if (str->link0) { \
			pos = str->value0; \
			goto label_1; \
		} \
		if (str->term0) { \
			value = str->value0; \
			goto label_2; \
		} \
	} \
}

static int inflate_decode(struct inflate *decode)
{
	uint8_t *input, *output, *lz77, v, v8;
	int check;
	unsigned x, pos, value, root1, root2, size;
	unsigned outputb, outputc, lz77now, lz77size;
	struct inflate_node *node1, *node2, *str;
	inflate_callback callback;
	void *instance;

	input = decode->input;
	output = decode->output;
	lz77 = decode->lz77;
	node1 = decode->node1;
	node2 = decode->node2;
	root1 = decode->huffman1.root;
	root2 = decode->huffman2.root;
	callback = decode->output_callback;
	instance = decode->output_instance;
	outputb = decode->outputb;
	outputc = decode->outputc;
	lz77now = decode->lz77now;
	lz77size = decode->lz77size;

	switch (decode->state_index) {
		case 0: goto huffman_begin;
		case 1: goto restore_1;
		case 2: goto restore_2;
		case 3: goto restore_3;
		case 4: goto restore_4;
		case 5: goto restore_5;
		case 6: goto restore_6;
		default: break;
	}
	InflateError("state_index error.");
	return -1;

huffman_begin:
	x = decode->inputx;
	InflateDebug(8U <= x, "inputx error.");
	if (x)
		v = input[decode->inputa];
	pos = root1;

huffman_loop:
	if (x == 0)
		goto huffman_read_0;
	else
		goto huffman_read_1;

restore_1:
	x = 0;
	pos = decode->decode_pos;

huffman_read_0:
	if (decode->inputc == 0) {
		decode->inputx = 0;
		decode->decode_pos = pos;
		decode->outputb = outputb;
		decode->outputc = outputc;
		decode->lz77now = lz77now;
		decode->lz77size = (lz77size < INFLATE_LZ77)? lz77size: INFLATE_LZ77;
		decode->state_index = 1;
		decode->input_call = 1;
		return 1;
	}
	v = input[decode->inputa];

huffman_read_1:
	check = ((v >> x) & 0x01);
	if (x < 7U) {
		x++;
	}
	else {
		x = 0;
		inflate_inputa_forward(decode, 1);
	}
	str = node1 + pos;
	Inflate_decode_tree(check, str, pos, value, huffman_loop, huffman_value);
	/* not found */
	InflateError("huffman code error.");
	return -1;

huffman_value:
	if (value < 256)
		goto huffman_byte;
	if (value == 256)
		goto huffman_end;
	if (value <= 285)
		goto lz77_begin;
	InflateError("Invalid huffman code.");
	return -1;

restore_2:
	x = decode->inputx;
	v = input[decode->inputa];
	value = decode->decode_value;

huffman_byte:
	if (InflateOutputFull(outputc)) {
		decode->inputx = x;
		decode->decode_value = value;
		decode->outputb = outputb;
		decode->outputc = outputc;
		decode->lz77now = lz77now;
		decode->lz77size = (lz77size < INFLATE_LZ77)? lz77size: INFLATE_LZ77;
		decode->state_index = 2;
		decode->output_call = 1;
		return 1;
	}
	/* output */
	v8 = (unsigned)value;
	output[outputb] = v8;
	outputb++;
	outputc++;
	if (INFLATE_RING2 <= outputb)
		outputb = 0;
	/* lz77 */
	lz77[lz77now++] = v8;
	if (INFLATE_LZ77 <= lz77now)
		lz77now = 0;
	lz77size++;
	/* callback */
	if (callback)
		(*callback)(instance, v8);
	/* loop */
	pos = root1;
	goto huffman_loop;

	/*
	 *  lz77
	 */
lz77_begin:
	decode->inputx = x;
	decode->outputb = outputb;
	decode->outputc = outputc;
	decode->lz77now = lz77now;
	decode->lz77size = (lz77size < INFLATE_LZ77)? lz77size: INFLATE_LZ77;
	goto lz77_extend1;

restore_3:
	value = decode->decode_value;

lz77_extend1:
	check = inflate_lz77_extend1(decode, 0, value, &value, &size);
	if (check < 0) {
		InflateError("inflate_lz77_extend1 error.");
		return -1;
	}
	if (check) {
		decode->decode_value = value;
		decode->state_index = 3;
		decode->input_call = 1;
		return 1;
	}
	decode->loop_size = value;
	inflate_move(decode, size);
	goto extend_begin;

extend_begin:
	x = decode->inputx;
	InflateDebug(8U <= x, "inputx error.");
	if (x)
		v = input[decode->inputa];
	pos = root2;

extend_loop:
	if (x == 0)
		goto extend_read_0;
	else
		goto extend_read_1;

restore_4:
	x = 0;
	pos = decode->decode_pos;

extend_read_0:
	if (decode->inputc == 0) {
		decode->inputx = 0;
		decode->decode_pos = pos;
		decode->state_index = 4;
		decode->input_call = 1;
		return 1;
	}
	v = input[decode->inputa];

extend_read_1:
	check = ((v >> x) & 0x01);
	if (x < 7U) {
		x++;
	}
	else {
		x = 0;
		inflate_inputa_forward(decode, 1);
	}
	str = node2 + pos;
	Inflate_decode_tree(check, str, pos, value, extend_loop, extend_value);
	/* not found */
	InflateError("extend code error.");
	return -1;

extend_value:
	decode->inputx = x;
	goto lz77_extend2;

restore_5:
	value = decode->decode_value;

lz77_extend2:
	check = inflate_lz77_extend2(decode, 0, value, &value, &size);
	if (check < 0) {
		InflateError("inflate_lz77_extend2 error.");
		return -1;
	}
	if (check) {
		decode->decode_value = value;
		decode->state_index = 5;
		decode->input_call = 1;
		return 1;
	}
	decode->loop_pos = value;
	decode->loop_now = 0;
	inflate_move(decode, size);

restore_6:
	check = inflate_decode_loop(decode);
	if (check < 0) {
		InflateError("inflate_decode_loop error.");
		return -1;
	}
	if (check) {
		decode->state_index = 6;
		decode->output_call = 1;
		return 1;
	}
	x = decode->inputx;
	v = input[decode->inputa];
	outputb = decode->outputb;
	outputc = decode->outputc;
	lz77now = decode->lz77now;
	lz77size = decode->lz77size;
	pos = root1;
	goto huffman_loop;

huffman_end:
	/* next */
	decode->inputx = x;
	decode->outputb = outputb;
	decode->outputc = outputc;
	decode->lz77now = lz77now;
	decode->lz77size = (lz77size < INFLATE_LZ77)? lz77size: INFLATE_LZ77;
	decode->state = Inflate_header;
	return 0;
}

static int inflate_flush(struct inflate *decode)
{
	InflateState("flush");
	if (decode->flush) {
		if (InflateOutputExist(decode->outputc)) {
			decode->output_call = 1;
			return 1;
		}
	}

	/* next */
	decode->state = Inflate_final;
	return 0;
}


/*
 *  Interface
 */
static int inflate_final(struct inflate *decode)
{
	InflateState("final");
	return 0;
}

static int inflate_failed(struct inflate *decode)
{
	InflateState("error");
	return -1;
}

typedef int (*inflate_state_call)(struct inflate *);
static inflate_state_call inflate_state_array[] = {
	inflate_start,
	inflate_header,
	inflate_plane,
	inflate_plane_pipe,
	inflate_fixed,
	inflate_dynamic,
	inflate_dynamic_header,
	inflate_dynamic_length,
	inflate_dynamic_value,
	inflate_decode,
	inflate_flush,
	inflate_final,
	inflate_failed
};

int inflate_execute(struct inflate *decode)
{
	int check;
	inflate_state_call call;

#ifdef HYPD_DEBUG
	call = inflate_state_array[Inflate_failed];
	InflateDebug(call != inflate_failed, "call error.");
#endif
	do {
		/* call */
		decode->input_call = 0;
		decode->output_call = 0;
		call = inflate_state_array[decode->state];
		check = (*call)(decode);

		/* result */
		if (check < 0) {
			decode->state = Inflate_failed;
			return -1;
		}
		if (check)
			return 1;
	}
	while (decode->state != Inflate_final);

	return 0;
}

int inflate_restart(struct inflate *decode)
{
	if (decode->state != Inflate_final) {
		InflateError("state error.");
		return -1;
	}

	/* restart */
	decode->state = Inflate_start;
	return 0;
}

