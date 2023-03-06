#include "deflate_zlib.h"
#include <stdio.h>
#include <string.h>

#define ZlibError(x) { \
	fprintf(stderr, "[ERROR] %s\n", x); \
}
#define ZlibError1(x,y) { \
	char __message[256]; \
	snprintf(__message, 256, x, y); \
	ZlibError(__message); \
}

/*****************************************************************************
 *  ADLER32
 *****************************************************************************/
#define ZLIB_ADLER32_INIT	1
#define ZLIB_ADLER32_BASE	65521

static void zlib_adler32_begin(struct zlib_adler32 *inst)
{
	uint32_t adler;

	adler = ZLIB_ADLER32_INIT;
	inst->s1 = adler & 0xFFFFU;
	inst->s2 = (adler >> 16U) & 0xFFFFU;
}

static void zlib_adler32_end(struct zlib_adler32 *inst, uint32_t *ret)
{
	*ret = (inst->s2 << 16U) + inst->s1;
}

static void zlib_adler32_putc(struct zlib_adler32 *inst, uint8_t v)
{
	inst->s1 = (inst->s1 + v) % ZLIB_ADLER32_BASE;
	inst->s2 = (inst->s2 + inst->s1) % ZLIB_ADLER32_BASE;
}


/*****************************************************************************
 *  decode
 *****************************************************************************/
void zlib_decode_init(struct zlib_decode *inst)
{
#ifdef HYPD_DEBUG
	memset(inst, 0xAA, sizeof(struct zlib_decode));
#endif
	inflate_init(&(inst->decode));
	inst->flush = 1;
	inst->state = ZlibDecode_start;
}

static int zlib_decode_start(struct zlib_decode *inst)
{
	struct inflate *decode;

	decode = &(inst->decode);
	decode->output_callback = NULL;
	decode->output_instance = NULL;
	inst->state_index = 0;
	inst->state = ZlibDecode_header;

	return 0;
}

const void *zlib_decode_input(struct zlib_decode *inst,
		const void *ptr, size_t size, size_t *ret)
{
	return inflate_input(&(inst->decode), ptr, size, ret);
}

void *zlib_decode_output(struct zlib_decode *inst,
		void *ptr, size_t size, size_t *ret)
{
	return inflate_output(&(inst->decode), ptr, size, ret);
}

static int zlib_decode_readvn(struct inflate *decode, unsigned size, unsigned *ret)
{
	uint8_t v;
	int check;
	unsigned i, value;

	/* Big endian */
	check = inflate_byte_read_p(decode, size);
	if (check < 0) {
		ZlibError("inflate_byte_read_p error.");
		return -1;
	}
	if (check)
		return 1;
	value = 0;
	for (i = 0; i < size; i++) {
		(void)inflate_byte_getc(decode, &v);
		value = (value << 8) | v;
	}
	*ret = value;

	return 0;
}

static int zlib_decode_readv4(struct inflate *decode, uint32_t *ret)
{
	int check;
	unsigned value;

	check = zlib_decode_readvn(decode, 4, &value);
	if (check)
		return check;
	*ret = (uint32_t)value;

	return 0;
}

static int zlib_decode_header(struct zlib_decode *inst)
{
	uint8_t x, y;
	uint32_t v4;
	unsigned z;
	int check;
	struct inflate *decode;

	decode = &(inst->decode);
	switch (inst->state_index) {
		case 0: goto header_bit;
		case 1: goto header_dictid;
		default: break;
	}
	/* error */
	ZlibError("state_index error.");
	return -1;

header_bit:
	check = inflate_byte_read_p(decode, 2);
	if (check < 0) {
		ZlibError("inflate_byte_read_p error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}

	/* CMF */
	check = inflate_byte_getc(decode, &x);
	if (check) {
		ZlibError("inflate_byte_getc CMF error.");
		return -1;
	}
	/* FLG */
	check = inflate_byte_getc(decode, &y);
	if (check) {
		ZlibError("inflate_byte_getc FLG error.");
		return -1;
	}
	/* structure */
	inst->header_cm = (x & 0x0F);
	inst->header_cinfo = (x >> 4);
	inst->header_fcheck = (y & 0x1F);
	inst->header_fdict = ((y >> 5) & 0x01);
	inst->header_flevel = (y >> 6);
	z = (x * 256) + y;
	if ((z % 31) != 0) {
		ZlibError("fcheck error.");
		return -1;
	}
	if (inst->header_cm != 8) {
		ZlibError("CMF is not deflate encoding.");
		return -1;
	}
	if (7 < inst->header_cinfo) {
		ZlibError1("CINFO must be smaller than equal to 7, but %u.",
				inst->header_cinfo);
		return -1;
	}
	inst->state_index = 1;

header_dictid:
	if (inst->header_fdict) {
		check = zlib_decode_readv4(decode, &v4);
		if (check < 0) {
			ZlibError("zlib_decode_readv4 error.");
			return check;
		}
		if (check) {
			decode->input_call = 1;
			return 1;
		}
		inst->dictid = v4;
	}

	/* next */
	inst->state = ZlibDecode_dictid;
	return 0;
}

static int zlib_decode_dictid(struct zlib_decode *inst)
{
	/* Preset check */
	if (inst->header_fdict) {
		ZlibError("DICTID is not supported.");
		return -1;
	}

	/* next */
	inst->state = ZlibDecode_block;
	return 0;
}

static void zlib_decode_callback(void *ptr, uint8_t v)
{
	zlib_adler32_putc((struct zlib_adler32 *)ptr, v);
}

static int zlib_decode_block(struct zlib_decode *inst)
{
	int check;
	struct inflate *decode;

	/* alignment */
	decode = &(inst->decode);
	check = inflate_alignment(decode);
	if (check)
		return check;

	/* adler32 */
	decode->output_callback = zlib_decode_callback;
	decode->output_instance = (void *)&(inst->adler32);
	zlib_adler32_begin(&(inst->adler32));

	/* next */
	inst->state = ZlibDecode_inflate;
	return 0;
}

static int zlib_decode_inflate(struct zlib_decode *inst)
{
	int check;
	struct inflate *decode;

	decode = &(inst->decode);
	check = inflate_execute(decode);
	if (check < 0) {
		ZlibError("inflate_execute error.");
		return -1;
	}
	if (check)
		return 1;

	/* next */
	decode->output_callback = NULL;
	decode->output_instance = NULL;
	inst->state = ZlibDecode_adler32;
	return 0;
}

static int zlib_decode_adler32(struct zlib_decode *inst)
{
	int check;
	uint32_t x, y;
	struct inflate *decode;

	/* input alignment */
	decode = &(inst->decode);
	if (inflate_alignment(decode)) {
		ZlibError("inflate_alignment error.");
		return -1;
	}

	/* adler32 check */
	decode = &(inst->decode);
	zlib_adler32_end(&(inst->adler32), &x);
	check = zlib_decode_readv4(decode, &y);
	if (check < 0) {
		ZlibError("zlib_decode_readv4 adler32 error.");
		return -1;
	}
	if (check) {
		decode->input_call = 1;
		return 1;
	}
	if (x != y) {
		ZlibError("adler32 error.");
		return -1;
	}

	/* next */
	inst->state = ZlibDecode_flush;
	return 0;
}

static int zlib_decode_flush(struct zlib_decode *inst)
{
	struct inflate *decode;

	/* flush */
	if (inst->flush) {
		decode = &(inst->decode);
		if (decode->outputc) {
			decode->output_call = 1;
			return 1;
		}
	}

	/* next */
	inst->state = ZlibDecode_final;
	return 0;
}

int zlib_decode_restart(struct zlib_decode *inst)
{
	struct inflate *decode;

	/* zlib */
	if (inst->state != ZlibDecode_final) {
		ZlibError("zlib_decode state error.");
		return -1;
	}

	/* inflate */
	decode = &(inst->decode);
	if (inflate_restart(decode)) {
		ZlibError("inflate state error.");
		return -1;
	}

	/* restart */
	inst->state = ZlibDecode_start;
	return 0;
}

static int zlib_decode_state(struct zlib_decode *inst)
{
	switch (inst->state) {
		case ZlibDecode_start:
			return zlib_decode_start(inst);

		case ZlibDecode_header:
			return zlib_decode_header(inst);

		case ZlibDecode_dictid:
			return zlib_decode_dictid(inst);

		case ZlibDecode_block:
			return zlib_decode_block(inst);

		case ZlibDecode_inflate:
			return zlib_decode_inflate(inst);

		case ZlibDecode_adler32:
			return zlib_decode_adler32(inst);

		case ZlibDecode_flush:
			return zlib_decode_flush(inst);

		case ZlibDecode_final:
			return 0;

		case ZlibDecode_failed:
		default:
			return -1;
	}
}

int zlib_decode_execute(struct zlib_decode *inst)
{
	int check;
	struct inflate *decode;

	/* call */
	decode = &(inst->decode);
	do {
		decode->input_call = 0;
		decode->output_call = 0;
		check = zlib_decode_state(inst);
		if (check < 0) {
			inst->state = ZlibDecode_failed;
			return -1;
		}
		if (check)
			return 1;
	}
	while (inst->state != ZlibDecode_final);

	return 0;
}


/*****************************************************************************
 *  encode
 *****************************************************************************/
void zlib_encode_init(struct zlib_encode *inst)
{
#ifdef HYPD_DEBUG
	memset(inst, 0xAA, sizeof(struct zlib_encode));
#endif
	deflate_init(&(inst->encode));
	inst->flush = 1;
	inst->header_fdict = 0;
	inst->dictid = 0;
	inst->state = ZlibEncode_start;
}

const void *zlib_encode_input(struct zlib_encode *inst,
		const void *ptr, size_t size, size_t *ret)
{
	return deflate_input(&(inst->encode), ptr, size, ret);
}

void *zlib_encode_output(struct zlib_encode *inst,
		void *ptr, size_t size, size_t *ret)
{
	return deflate_output(&(inst->encode), ptr, size, ret);
}

static int zlib_encode_start(struct zlib_encode *inst)
{
	inst->state_index = 0;
	inst->state = ZlibEncode_header;
	return 0;
}

static int zlib_encode_writevn(struct deflate *encode, unsigned size, unsigned value)
{
	uint8_t v;
	int check;
	unsigned i;

	/* Big endian */
	check = deflate_byte_write_p(encode, size);
	if (check < 0) {
		ZlibError("deflate_byte_write_p error.");
		return -1;
	}
	if (check)
		return 1;
	for (i = 0; i < size; i++) {
		v = (value >> ((size - i - 1) * 8)) & 0xFF;
		(void)deflate_byte_putc(encode, v);
	}

	return 0;
}

static int zlib_encode_writev4(struct deflate *encode, uint32_t value)
{
	return zlib_encode_writevn(encode, 4, (unsigned)value);
}

static int zlib_encode_header(struct zlib_encode *inst)
{
	uint8_t x, y;
	unsigned z;
	int check;
	struct deflate *encode;

	encode = &(inst->encode);
	switch (inst->state_index) {
		case 0: goto header_bit;
		case 1: goto header_dictid;
		default: break;
	}
	/* error */
	ZlibError("state_index error.");
	return -1;

header_bit:
	check = deflate_byte_write_p(encode, 2);
	if (check < 0) {
		ZlibError("deflate_byte_write_p error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}

	/* CMF */
	x = 0x78; /* 7=32KByte, 8=deflate */
	check = deflate_byte_putc(encode, x);
	if (check) {
		ZlibError("deflate_byte_putc CMF error.");
		return -1;
	}

	/* FLG */
	y = 0;
	/* fdict */
	if (inst->header_fdict)
		y |= 0x10;  /* 5bit */
	/* flevel */
	switch (encode->mode) {
		case deflate_mode_plane_only:
			y |= (0 << 6); break;
		case deflate_mode_fixed_only:
			y |= (0 << 6); break;
		case deflate_mode_dynamic_only:
			y |= (2 << 6); break;
		case deflate_mode_fast:
			y |= (1 << 6); break;
		case deflate_mode_default:
			y |= (2 << 6); break;
		case deflate_mode_slow:
			y |= (3 << 6); break;
		default:
			ZlibError("Invalid mode.");
			return -1;
	}
	/* fcheck */
	z = (x * 256) + y;
	z = z % 31;
	if (z)
		z = 31 - z;
	y |= (uint8_t)z;
	/* write */
	check = deflate_byte_putc(encode, y);
	if (check) {
		ZlibError("deflate_byte_putc FLG error.");
		return -1;
	}
	inst->state_index = 1;

header_dictid:
	if (inst->header_fdict) {
		check = zlib_encode_writev4(encode, inst->dictid);
		if (check < 0) {
			ZlibError("zlib_encode_writev4 error.");
			return check;
		}
		if (check) {
			encode->output_call = 1;
			return 1;
		}
	}

	/* next */
	inst->state_index = 0;
	inst->state = ZlibEncode_block;
	return 0;
}

static void zlib_encode_callback(void *ptr, uint8_t v)
{
	zlib_adler32_putc((struct zlib_adler32 *)ptr, v);
}

static int zlib_encode_block(struct zlib_encode *inst)
{
	int check;
	struct deflate *encode;

	/* alignment */
	encode = &(inst->encode);
	check = deflate_alignment(encode);
	if (check < 0) {
		ZlibError("deflate_alignment error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}

	/* adler32 */
	encode->input_callback = zlib_encode_callback;
	encode->input_instance = (void *)&(inst->adler32);
	zlib_adler32_begin(&(inst->adler32));

	/* next */
	inst->state = ZlibEncode_deflate;
	return 0;
}

static int zlib_encode_deflate(struct zlib_encode *inst)
{
	int check;
	struct deflate *encode;

	encode = &(inst->encode);
	check = deflate_execute(encode);
	if (check < 0) {
		ZlibError("deflate_execute error.");
		return -1;
	}
	if (check)
		return check;

	/* next */
	encode->input_callback = NULL;
	encode->input_instance = NULL;
	inst->state = ZlibEncode_adler32;
	return 0;
}

static int zlib_encode_adler32(struct zlib_encode *inst)
{
	int check;
	uint32_t x;
	struct deflate *encode;

	/* adler32 check */
	encode = &(inst->encode);
	check = deflate_byte_write_p(encode, 4);
	if (check < 0) {
		ZlibError("deflate_byte_write_p error.");
		return -1;
	}
	if (check) {
		encode->output_call = 1;
		return 1;
	}

	zlib_adler32_end(&(inst->adler32), &x);
	(void)zlib_encode_writev4(encode, x);

	/* next */
	inst->state = ZlibEncode_flush;
	return 0;
}

static int zlib_encode_flush(struct zlib_encode *inst)
{
	struct deflate *encode;

	if (inst->flush) {
		encode = &(inst->encode);
		if (encode->outputc) {
			encode->output_call = 1;
			return 1;
		}
	}

	/* next */
	inst->state = ZlibEncode_final;
	return 0;
}

static int zlib_encode_state(struct zlib_encode *inst)
{
	switch (inst->state) {
		case ZlibEncode_start:
			return zlib_encode_start(inst);

		case ZlibEncode_header:
			return zlib_encode_header(inst);

		case ZlibEncode_block:
			return zlib_encode_block(inst);

		case ZlibEncode_deflate:
			return zlib_encode_deflate(inst);

		case ZlibEncode_adler32:
			return zlib_encode_adler32(inst);

		case ZlibEncode_flush:
			return zlib_encode_flush(inst);

		case ZlibEncode_final:
			return 0;

		case ZlibEncode_failed:
		default:
			return -1;
	}
}

int zlib_encode_execute(struct zlib_encode *inst)
{
	int check;
	struct deflate *encode;

	encode = &(inst->encode);
	do {
		encode->input_call = 0;
		encode->output_call = 0;
		check = zlib_encode_state(inst);
		if (check < 0) {
			inst->state = ZlibEncode_failed;
			return -1;
		}
		if (check)
			return 1;
	}
	while (inst->state != ZlibEncode_final);

	return 0;
}

int zlib_encode_break(struct zlib_encode *inst)
{
	return deflate_break(&(inst->encode));
}

int zlib_encode_restart(struct zlib_encode *inst)
{
	if (inst->state != ZlibEncode_final) {
		ZlibError("zlib state error.");
		return -1;
	}
	if (deflate_break(&(inst->encode)))
		return -1;

	/* restart */
	inst->state = ZlibEncode_start;
	return 0;
}

