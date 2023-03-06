#include <stdlib.h>
#include <string.h>
#include "unicode.h"

/*****************************************************************************
 *  UTF-8
 *****************************************************************************/
void init_utf8(struct state_utf8 *ptr)
{
#ifdef HYPD_DEBUG
	memset(ptr, 0xAA, sizeof(struct state_utf8));
#endif
	ptr->state = 0;
}

int decode_utf8(struct state_utf8 *ptr, uint8_t c, unicode *ret)
{
	unicode v;

	switch (ptr->state) {
		case 0:
			goto sequence0;
		case 1:
			goto sequence2_1;
		case 2:
			goto sequence3_1;
		case 3:
			goto sequence3_2;
		case 4:
			goto sequence4_1;
		case 5:
			goto sequence4_2;
		case 6:
			goto sequence4_3;
		case -1:
			goto error_result;
		default:
			goto error_switch;
	}

sequence0:
	if (c <= 0x7F)
		goto sequence1;
	if (0xC2 <= c && c <= 0xDF)
		goto sequence2;
	if (0xE0 <= c && c <= 0xEF)
		goto sequence3;
	if (0xF0 <= c && c <= 0xF4)
		goto sequence4;
	goto error_unicode;

sequence1:
	*ret = (unicode)c;
	return 1;

sequence2:
	ptr->value = (0x1F & c) << 6;
	ptr->state = 1;
	return 0;

sequence2_1:
	if (c < 0x80 || 0xBF < c)
		goto error_unicode;
	*ret = ptr->value | (0x3F & c);
	ptr->state = 0;
	return 1;

sequence3:
	ptr->value = (0x0F & c) << 12;
	ptr->state = 2;
	return 0;

sequence3_1:
	if (c < 0x80 || 0xBF < c)
		goto error_unicode;
	ptr->value |= (0x3F & c) << 6;
	ptr->state = 3;
	return 0;

sequence3_2:
	if (c < 0x80 || 0xBF < c)
		goto error_unicode;
	v = ptr->value | (0x3F & c);
	if (v < 0x0800)
		goto error_range;
	if (0xD800 <= v && v <= 0xDFFF)
		goto error_surrogate;
	*ret = v;
	ptr->state = 0;
	return 1;

sequence4:
	ptr->value = (0x07 & c) << 18;
	ptr->state = 4;
	return 0;

sequence4_1:
	if (c < 0x80 || 0xBF < c)
		goto error_unicode;
	ptr->value |= (0x3F & c) << 12;
	ptr->state = 5;
	return 0;

sequence4_2:
	if (c < 0x80 || 0xBF < c)
		goto error_unicode;
	ptr->value |= (0x3F & c) << 6;
	ptr->state = 6;
	return 0;

sequence4_3:
	if (c < 0x80 || 0xBF < c)
		goto error_unicode;
	v = ptr->value | (0x3F & c);
	if (v < 0x010000)
		goto error_range;
	if (0x110000 <= v)
		goto error_range;
	*ret = v;
	ptr->state = 0;
	return 1;

error_switch:
	ptr->error = 1;
	goto error_result;

error_unicode:
	ptr->error = 2;
	goto error_result;

error_range:
	ptr->error = 3;
	goto error_result;

error_surrogate:
	ptr->error = 4;
	goto error_result;

error_result:
	*ret = 0;
	ptr->state = -1;
	return -1;
}

int encode_utf8(unicode c, uint8_t *dst, int *ret)
{
	int w;

	/* minus */
	if (c < 0)
		goto error;

	/* 1 byte */
	w = 0;
	if (c < 0x80) {
		dst[w++] = c;
		goto normal;
	}

	/* 2 byte */
	if (c < 0x0800) {
		dst[w++] = 0xC2 | (c >> 6);
		dst[w++] = 0x80 | (0x3F & c);
		goto normal;
	}

	/* 3 byte */
	if (c < 0xD800)
		goto sequence3;

	/* surrogate pair */
	if (c < 0xE000)
		goto error;

	/* 3 byte */
	if (c < 0x010000) {
sequence3:
		dst[w++] = 0xE0 | (c >> 12);
		dst[w++] = 0x80 | (0x3F & (c >> 6));
		dst[w++] = 0x80 | (0x3F & c);
		goto normal;
	}

	/* 4 byte */
	if (c < 0x110000) {
		dst[w++] = 0xF0 | (c >> 18);
		dst[w++] = 0x80 | (0x3F & (c >> 12));
		dst[w++] = 0x80 | (0x3F & (c >> 6));
		dst[w++] = 0x80 | (0x3F & c);
		goto normal;
	}

	/* error */
error:
	*ret = 0;
	return 1;

normal:
	*ret = w;
	return 0;
}

int length_utf8(unicode c, unsigned *ret)
{
	if (c < 0)         { *ret = 0; return 1; }
	if (c < 0x80)      { *ret = 1; return 0; }
	if (c < 0x0800)    { *ret = 2; return 0; }
	if (c < 0xD800)    { *ret = 3; return 0; }
	if (c < 0xE000)    { *ret = 0; return 1; } /* error */
	if (c < 0x010000)  { *ret = 3; return 0; }
	if (c < 0x110000)  { *ret = 4; return 0; }
	else               { *ret = 0; return 1; }
}


/*****************************************************************************
 *  UTF-16
 *****************************************************************************/
void init_utf16(struct state_utf16 *ptr)
{
#ifdef HYPD_DEBUG
	memset(ptr, 0xAA, sizeof(struct state_utf16));
#endif
	ptr->state = 0;
}

int decode_utf16(struct state_utf16 *ptr, uint16_t c, unicode *ret)
{
	unicode a;

	switch (ptr->state) {
		case 0:
			goto sequence0;
		case 1:
			goto sequence1;
		case -1:
			goto error_result;
		default:
			goto error_switch;
	}

sequence0:
	if (0xD800 <= c && c <= 0xDBFF) {
		ptr->value = c;
		ptr->state = 1;
		return 0;
	}
	if (0xDC00 <= c && c <= 0xDFFF)
		goto error_first;
	*ret = (unicode)c;
	ptr->state = 0;
	return 1;

sequence1:
	if (c < 0xDC00 || 0xE000 <= c)
		goto error_second;
	a = ptr->value;
	*ret = ((((a >> 6) & 0x0F) + 1) << 16)
		| ((a & 0x3F) << 10)
		| (c & 0x03FF);
	ptr->state = 0;
	return 1;

error_switch:
	ptr->error = 1;
	goto error_result;

error_first:
	ptr->error = 2;
	goto error_result;

error_second:
	ptr->error = 3;
	goto error_result;

error_result:
	*ret = 0;
	ptr->state = -1;
	return -1;
}

int encode_utf16(unicode c, uint16_t *dst, int *ret)
{
	unicode x;

	/* minus */
	if (c < 0)
		goto error;

	/* 1 byte, 2 byte */
	if (c < 0xD800) {
		dst[0] = (uint16_t)c;
		*ret = 1;
		return 0;
	}

	/* surrogate pair */
	if (c < 0xE000)
		goto error;

	/* 2 byte */
	if (c < 0x010000) {
		dst[0] = (uint16_t)c;
		*ret = 1;
		return 0;
	}

	/* 4 byte */
	if (c < 0x110000) {
		x = ((((c >> 16) & 0x1F) - 1) << 6) | ((c >> 10) & 0x3F);
		dst[0] = 0xD800 | x;
		dst[1] = 0xDC00 | (0x03FF & c);
		*ret = 2;
		return 0;
	}

	/* error */
error:
	*ret = 0;
	return 1;
}

int length_utf16(unicode c, unsigned *ret)
{
	if (c < 0)         { *ret = 0; return 1; }
	if (c < 0xD800)    { *ret = 1; return 0; }
	if (c < 0xE000)    { *ret = 0; return 1; } /* surrogate pair */
	if (c < 0x010000)  { *ret = 1; return 0; }
	if (c < 0x110000)  { *ret = 2; return 0; }
	else               { *ret = 0; return 1; }
}


/*****************************************************************************
 *  UTF-16LE
 *****************************************************************************/
void init_utf16le(struct state_utf16le *ptr)
{
#ifdef HYPD_DEBUG
	memset(ptr, 0xAA, sizeof(struct state_utf16le));
#endif
	init_utf16(&(ptr->decode));
	ptr->state = 0;
}

int decode_utf16le(struct state_utf16le *ptr, uint8_t c, unicode *ret)
{
	int check;
	uint16_t x;
	unicode u;

	switch (ptr->state) {
		case 0:
			goto sequence0;
		case 1:
			goto sequence1;
		case 2:
			goto sequence2;
		case 3:
			goto sequence3;
		case -1:
			goto error_result;
		default:
			goto error_switch;
	}

sequence0:
	ptr->value = c;
	ptr->state = 1;
	return 0;

sequence1:
	x = ptr->value | (c << 8);
	check = decode_utf16(&(ptr->decode), x, &u);
	if (check < 0)
		goto error_sequence1;
	if (! check) {
		ptr->state = 2;
		return 0;
	}
	*ret = u;
	init_utf16le(ptr);
	return 1;

sequence2:
	ptr->value = c;
	ptr->state = 3;
	return 0;

sequence3:
	x = ptr->value | (c << 8);
	check = decode_utf16(&(ptr->decode), x, &u);
	if (check < 0)
		goto error_sequence3;
	if (! check)
		goto error_decode;
	*ret = u;
	init_utf16le(ptr);
	return 1;

error_switch:
	ptr->error = 1;
	goto error_result;

error_sequence1:
	ptr->error = 2;
	goto error_result;

error_sequence3:
	ptr->error = 3;
	goto error_result;

error_decode:
	ptr->error = 4;
	goto error_result;

error_result:
	*ret = 0;
	ptr->state = -1;
	return -1;
}

int encode_utf16le(unicode c, uint8_t *dst, int *ret)
{
	uint16_t code[2], x;
	int size, i;

	if (encode_utf16(c, code, &size)) {
		*ret = 0;
		return 1;
	}
	i = 0;
	x = code[0];
	dst[i++] = 0xFF & x;
	dst[i++] = 0xFF & (x >> 8);
	if (2 <= size) {
		x = code[1];
		dst[i++] = 0xFF & x;
		dst[i++] = 0xFF & (x >> 8);
	}

	*ret = i;
	return 0;
}

int length_utf16le(unicode c, unsigned *ret)
{
	unsigned size;

	if (length_utf16(c, &size)) {
		*ret = 0;
		return 1;
	}
	*ret = size * 2;
	return 0;
}


/*****************************************************************************
 *  UTF-16BE
 *****************************************************************************/
void init_utf16be(struct state_utf16be *ptr)
{
#ifdef HYPD_DEBUG
	memset(ptr, 0xAA, sizeof(struct state_utf16be));
#endif
	init_utf16(&(ptr->decode));
	ptr->state = 0;
}

int decode_utf16be(struct state_utf16be *ptr, uint8_t c, unicode *ret)
{
	int check;
	uint16_t x;
	unicode u;

	switch (ptr->state) {
		case 0:
			goto sequence0;
		case 1:
			goto sequence1;
		case 2:
			goto sequence2;
		case 3:
			goto sequence3;
		case -1:
			goto error_result;
		default:
			goto error_switch;
	}

sequence0:
	ptr->value = c;
	ptr->state = 1;
	return 0;

sequence1:
	x = (ptr->value << 8) | c;
	check = decode_utf16(&(ptr->decode), x, &u);
	if (check < 0)
		goto error_sequence1;
	if (! check) {
		ptr->state = 2;
		return 0;
	}
	*ret = u;
	init_utf16be(ptr);
	return 1;

sequence2:
	ptr->value = c;
	ptr->state = 3;
	return 0;

sequence3:
	x = (ptr->value << 8) | c;
	check = decode_utf16(&(ptr->decode), x, &u);
	if (check < 0)
		goto error_sequence3;
	if (! check)
		goto error_decode;
	*ret = u;
	init_utf16be(ptr);
	return 1;

error_switch:
	ptr->error = 1;
	goto error_result;

error_sequence1:
	ptr->error = 2;
	goto error_result;

error_sequence3:
	ptr->error = 3;
	goto error_result;

error_decode:
	ptr->error = 4;
	goto error_result;

error_result:
	*ret = 0;
	ptr->state = -1;
	return -1;
}

int encode_utf16be(unicode c, uint8_t *dst, int *ret)
{
	uint16_t code[2], x;
	int size, i;

	if (encode_utf16(c, code, &size)) {
		*ret = 0;
		return 1;
	}
	i = 0;
	x = code[0];
	dst[i++] = 0xFF & (x >> 8);
	dst[i++] = 0xFF & x;
	if (2 <= size) {
		x = code[1];
		dst[i++] = 0xFF & (x >> 8);
		dst[i++] = 0xFF & x;
	}

	*ret = i;
	return 0;
}

int length_utf16be(unicode c, unsigned *ret)
{
	unsigned size;

	if (length_utf16(c, &size)) {
		*ret = 0;
		return 1;
	}
	*ret = size * 2;
	return 0;
}


/*****************************************************************************
 *  UTF-32
 *****************************************************************************/
int length_utf32(unicode c, unsigned *ret)
{
	if (c < 0)         { *ret = 0; return 1; }
	if (c < 0xD800)    { *ret = 1; return 0; }
	if (c < 0xE000)    { *ret = 0; return 1; } /* surrogate pair */
	if (c < 0x110000)  { *ret = 1; return 0; }
	else               { *ret = 0; return 1; }
}


/*****************************************************************************
 *  UTF-32LE
 *****************************************************************************/
void init_utf32le(struct state_utf32le *ptr)
{
#ifdef HYPD_DEBUG
	memset(ptr, 0xAA, sizeof(struct state_utf32le));
#endif
	ptr->state = 0;
}

int decode_utf32le(struct state_utf32le *ptr, uint8_t c, unicode *ret)
{
	unicode u;

	switch (ptr->state) {
		case 0:
			goto sequence0;
		case 1:
			goto sequence1;
		case 2:
			goto sequence2;
		case 3:
			goto sequence3;
		case -1:
			goto error_result;
		default:
			goto error_switch;
	}

sequence0:
	ptr->value = c;
	ptr->state = 1;
	return 0;

sequence1:
	ptr->value |= (c << 8);
	ptr->state = 2;
	return 0;

sequence2:
	ptr->value |= (c << 16);
	ptr->state = 3;
	return 0;

sequence3:
	u = ptr->value | (c << 24);
	if (u < 0 || 0x110000 <= u)
		goto error_value;
	if (0xD800 <= u && u < 0xE000)
		goto error_surrogate;
	*ret = u;
	ptr->state = 0;
	return 1;

error_switch:
	ptr->error = 1;
	goto error_result;

error_value:
	ptr->error = 2;
	goto error_result;

error_surrogate:
	ptr->error = 3;
	goto error_result;

error_result:
	*ret = 0;
	ptr->state = -1;
	return -1;
}

int encode_utf32le(unicode c, uint8_t *dst, int *ret)
{
	/* error */
	if (c < 0 || (0xD800 <= c && c < 0xE000) || 0x110000 <= c) {
		*ret = 0;
		return 1;
	}

	/* encode */
	dst[0] = c & 0xFF;
	dst[1] = (c >> 8) & 0xFF;
	dst[2] = (c >> 16) & 0xFF;
	dst[3] = (c >> 24) & 0xFF;
	*ret = 4;
	return 0;
}

int length_utf32le(unicode c, unsigned *ret)
{
	unsigned size;

	if (length_utf32(c, &size)) {
		*ret = 0;
		return 1;
	}
	*ret = size * 4;
	return 0;
}


/*****************************************************************************
 *  UTF-32BE
 *****************************************************************************/
void init_utf32be(struct state_utf32be *ptr)
{
#ifdef HYPD_DEBUG
	memset(ptr, 0xAA, sizeof(struct state_utf32be));
#endif
	ptr->state = 0;
}

int decode_utf32be(struct state_utf32be *ptr, uint8_t c, unicode *ret)
{
	unicode u;

	switch (ptr->state) {
		case 0:
			goto sequence0;
		case 1:
			goto sequence1;
		case 2:
			goto sequence2;
		case 3:
			goto sequence3;
		case -1:
			goto error_result;
		default:
			goto error_switch;
	}

sequence0:
	ptr->value = (c << 24);
	ptr->state = 1;
	return 0;

sequence1:
	ptr->value |= (c << 16);
	ptr->state = 2;
	return 0;

sequence2:
	ptr->value |= (c << 8);
	ptr->state = 3;
	return 0;

sequence3:
	u = ptr->value | c;
	if (u < 0 || 0x110000 <= u)
		goto error_value;
	if (0xD800 <= u && u < 0xE000)
		goto error_surrogate;
	*ret = u;
	ptr->state = 0;
	return 1;

error_switch:
	ptr->error = 1;
	goto error_result;

error_value:
	ptr->error = 2;
	goto error_result;

error_surrogate:
	ptr->error = 3;
	goto error_result;

error_result:
	*ret = 0;
	ptr->state = -1;
	return -1;
}

int encode_utf32be(unicode c, uint8_t *dst, int *ret)
{
	/* error */
	if (c < 0 || (0xD800 <= c && c < 0xE000) || 0x110000 <= c) {
		*ret = 0;
		return 1;
	}

	/* encode */
	dst[0] = (c >> 24) & 0xFF;
	dst[1] = (c >> 16) & 0xFF;
	dst[2] = (c >> 8) & 0xFF;
	dst[3] = c & 0xFF;
	*ret = 4;
	return 0;
}

int length_utf32be(unicode c, unsigned *ret)
{
	unsigned size;

	if (length_utf32(c, &size)) {
		*ret = 0;
		return 1;
	}
	*ret = size * 4;
	return 0;
}


/*****************************************************************************
 *  Byte Order Mark, BOM
 *****************************************************************************/
void init_byte_order_mark(struct byte_order_mark *ptr)
{
	memset(ptr, 0, sizeof(struct byte_order_mark));
}

int getc_byte_order_mark(struct byte_order_mark *ptr, int i, uint8_t *ret)
{
	int index;

	index = ptr->index;
	if (i < 0 || index <= i) {
		*ret = 0;
		return 1;  /* error */
	}
	else {
		*ret = ptr->data[i];
		return 0;
	}
}

static int putc_byte_order_mark_static(struct byte_order_mark *ptr, uint8_t c)
{
	if (BYTE_ORDER_MARK_HISTORY <= ptr->index)
		return 1;
	ptr->data[ptr->index] = c;
	ptr->index++;
	return 0;
}

enum byte_order_mark_type putc_byte_order_mark(struct byte_order_mark *ptr, uint8_t c)
{
	if (putc_byte_order_mark_static(ptr, c))
		goto error;

	switch (ptr->state) {
		case 0:
			goto init;
		case 1:
			goto finish;
		case 2:
			goto binary;
		case 3:
			goto utf8_1;
		case 4:
			goto utf16be_1;
		case 5:
			goto utf32be_1;
		case 6:
			goto utf16le_utf32le_1;
		case 7:
			goto utf8_2;
		case 8:
			goto utf32be_2;
		case 9:
			goto utf32be_3;
		case 10:
			goto utf16le_utf32le_2;
		case 11:
			goto utf16le_utf32le_3;
		case -1:
		default:
			goto error;
	}

init:
	if (c == 0xEF) {
		ptr->state = 3; /* utf8_1 */
		goto next;
	}
	if (c == 0xFE) {
		ptr->state = 4; /* utf16be_1 */
		goto next;
	}
	if (c == 0x00) {
		ptr->state = 5; /* utf32be_1 */
		goto next;
	}
	if (c == 0xFF) {
		ptr->state = 6; /* utf16le_utf32le_1 */
		goto next;
	}
	if (c == 0x2B) {
		/* UTF-7 is not supported */
		goto binary;
	}
	goto binary;

utf8_1:
	if (c == 0xBB) {
		ptr->state = 7; /* utf8_2 */
		goto next;
	}
	goto binary;

utf8_2:
	if (c == 0xBF) {
		ptr->type = byte_order_mark_UTF8;
		goto finish;
	}
	goto binary;

utf16be_1:
	if (c == 0xFF) {
		ptr->type = byte_order_mark_UTF16BE;
		goto finish;
	}
	goto binary;

utf32be_1:
	if (c == 0x00) {
		ptr->state = 8; /* utf32be_2 */
		goto next;
	}
	goto binary;

utf32be_2:
	if (c == 0xFE) {
		ptr->state = 9; /* utf32be_3 */
		goto next;
	}
	goto binary;

utf32be_3:
	if (c == 0xFF) {
		ptr->type = byte_order_mark_UTF32BE;
		goto finish;
	}
	goto binary;

utf16le_utf32le_1:
	if (c == 0xFE) {
		ptr->state = 10; /* utf16le_utf32le_2 */
		goto next;
	}
	goto binary;

utf16le_utf32le_2:
	if (c == 0x00) {
		ptr->state = 11; /*  utf16le_utf32le_3 */
		goto next;
	}
	else {
		ptr->type = byte_order_mark_UTF16LE;
		goto finish;
	}

utf16le_utf32le_3:
	if (c == 0x00)
		ptr->type = byte_order_mark_UTF32LE;
	else
		ptr->type = byte_order_mark_UTF16LE;
	goto finish;

next:
	ptr->type = byte_order_mark_next;
	return ptr->type;

error:
	ptr->state = -1; /* error */
	ptr->type = byte_order_mark_error;
	return ptr->type;

binary:
	ptr->state = 2; /* binary */
	ptr->type = byte_order_mark_binary;
	return ptr->type;

finish:
	ptr->state = 1; /* finish */
	return ptr->type;
}

enum byte_order_mark_type result_byte_order_mark(struct byte_order_mark *ptr)
{
	return ptr->type;
}


/*****************************************************************************
 *  stdio
 *****************************************************************************/
static int fgetc_unicode(void *ptr, FILE *file, unicode *ret,
		int (*call)(void *, uint8_t, unicode *))
{
	int c, check;
	unicode value;

	/* first */
	c = fgetc(file);
	if (c == EOF) {
		*ret = -1;
		return 0;
	}
	check = (*call)(ptr, (uint8_t)c, &value);
	if (check < 0) {
		*ret = 0;
		return 1;
	}
	if (check) {
		*ret = value;
		return 0;
	}

	/* second */
	for (;;) {
		c = fgetc(file);
		if (c == EOF) {
			*ret = 0;
			return 1;  /* decode error */
		}
		check = (*call)(ptr, (uint8_t)c, &value);
		if (check < 0) {
			*ret = 0;
			return 1;
		}
		if (check)
			break;
	}
	*ret = value;
	return 0;
}

static int fgetc_callback_utf8(void *ptr, uint8_t c, unicode *ret)
{
	return decode_utf8((struct state_utf8 *)ptr, c, ret);
}
int fgetc_utf8(FILE *file, unicode *ret)
{
	struct state_utf8 x;
	init_utf8(&x);
	return fgetc_unicode(&x, file, ret, fgetc_callback_utf8);
}

static int fgetc_callback_utf16le(void *ptr, uint8_t c, unicode *ret)
{
	return decode_utf16le((struct state_utf16le *)ptr, c, ret);
}
int fgetc_utf16le(FILE *file, unicode *ret)
{
	struct state_utf16le x;
	init_utf16le(&x);
	return fgetc_unicode(&x, file, ret, fgetc_callback_utf16le);
}

static int fgetc_callback_utf16be(void *ptr, uint8_t c, unicode *ret)
{
	return decode_utf16be((struct state_utf16be *)ptr, c, ret);
}
int fgetc_utf16be(FILE *file, unicode *ret)
{
	struct state_utf16be x;
	init_utf16be(&x);
	return fgetc_unicode(&x, file, ret, fgetc_callback_utf16be);
}

static int fgetc_callback_utf32le(void *ptr, uint8_t c, unicode *ret)
{
	return decode_utf32le((struct state_utf32le *)ptr, c, ret);
}
int fgetc_utf32le(FILE *file, unicode *ret)
{
	struct state_utf32le x;
	init_utf32le(&x);
	return fgetc_unicode(&x, file, ret, fgetc_callback_utf32le);
}

static int fgetc_callback_utf32be(void *ptr, uint8_t c, unicode *ret)
{
	return decode_utf32be((struct state_utf32be *)ptr, c, ret);
}
int fgetc_utf32be(FILE *file, unicode *ret)
{
	struct state_utf32be x;
	init_utf32be(&x);
	return fgetc_unicode(&x, file, ret, fgetc_callback_utf32be);
}

static int fputc_unicode(unicode c, FILE *file,
		int (*call)(unicode, uint8_t *, int *))
{
	uint8_t array[8];
	int size, i;

	if ((*call)(c, array, &size))
		return 1;
	for (i = 0; i < size; i++) {
		if (fputc(array[i], file) < 0)
			return 1;
	}

	return 0;
}

int fputc_utf8(unicode c, FILE *file)
{
	return fputc_unicode(c, file, encode_utf8);
}

int fputc_utf16le(unicode c, FILE *file)
{
	return fputc_unicode(c, file, encode_utf16le);
}

int fputc_utf16be(unicode c, FILE *file)
{
	return fputc_unicode(c, file, encode_utf16be);
}

int fputc_utf32le(unicode c, FILE *file)
{
	return fputc_unicode(c, file, encode_utf32le);
}

int fputc_utf32be(unicode c, FILE *file)
{
	return fputc_unicode(c, file, encode_utf32be);
}


/*****************************************************************************
 *  memory21
 *****************************************************************************/
size_t byte_size_memory21(size_t unicode_size)
{
	size_t a, b, c;

	a = unicode_size * 21UL;
	b = a / 8UL;
	c = a % 8UL;
	if (c != 0)
		b++;

	return b;
}

size_t unicode_size_memory21(size_t byte_size)
{
	return (byte_size * 8UL) / 21UL;
}

void clear_memory21(void *ptr, size_t unicode_size)
{
	size_t byte_size;

	byte_size = byte_size_memory21(unicode_size);
	memset(ptr, 0, byte_size);
}

void getc_memory21(const void *ptr, size_t i, unicode *ret)
{
	unsigned a, b;
	const uint8_t *pos;
	uint8_t v;
	unicode c;
	size_t x1, x2, x3;

	x1 = i * 21;
	x2 = x1 / 8;
	x3 = x1 % 8;
	pos = (uint8_t *)ptr;
	a = 21;
	b = 0;
	c = 0;

	/* first */
	if (x3 != 0) {
		c = pos[x2++] >> x3;
		b = 8U - x3;
		a -= b;
	}

	/* byte */
	while (8 < a) {
		c |= ((unicode)pos[x2++]) << b;
		a -= 8;
		b += 8;
	}

	/* end */
	if (a != 0) {
		a = 8U - a;
		v = pos[x2];
		v <<= a;
		v >>= a;
		c |= ((unicode)v) << b;
	}

	/* result */
	*ret = c;
}

void putc_memory21(void *ptr, size_t i, unicode c)
{
	unsigned a;
	uint8_t *pos, low, high;
	size_t x1, x2, x3, x4;

	c &= 0x01FFFFF;
	x1 = i * 21;
	x2 = x1 / 8;
	x3 = x1 % 8;
	pos = (uint8_t *)ptr;
	a = 21;

	/* first */
	if (x3 != 0) {
		high = c << x3;
		x4 = 8U - x3;
		low = pos[x2];
		low <<= x4;
		low >>= x4;
		pos[x2++] = high | low;
		c >>= x4;
		a -= (unsigned)x4;
	}

	/* byte */
	while (8 < a) {
		pos[x2++] = 0xFFU & c;
		c >>= 8;
		a -= 8;
	}

	/* end */
	if (a != 0) {
		high = pos[x2];
		high >>= a;
		high <<= a;
		low = 0xFFU & c;
		pos[x2] = high | low;
	}
}

int compare_memory21(const void *x, const void *y, size_t unicode_size)
{
	size_t i;
	unicode a, b;

	for (i = 0; i < unicode_size; i++) {
		getc_memory21(x, i, &a);
		getc_memory21(y, i, &b);
		if (a < b)
			return -1;
		if (a > b)
			return 1;
	}

	return 0;
}

int equal_memory21(const void *x, const void *y, size_t unicode_size)
{
	size_t a, b, c, last;
	unicode z, w;

	if (unicode_size == 0)
		return 1; /* equal */

	a = unicode_size * 21UL;
	b = a / 8UL;
	if (memcmp(x, y, b) != 0)
		return 0; /* not */

	c = a % 8UL;
	if (c == 0)
		return 1; /* equal */

	last = unicode_size - 1UL;
	getc_memory21(x, last, &z);
	getc_memory21(y, last, &w);
	return z == w;
}


/*****************************************************************************
 *  string21
 *****************************************************************************/
static string21 *make_string21_static(size_t size, int clear_p)
{
	string21 *str;
	void *ptr;
	size_t alloc;

	str = (string21 *)string21_malloc(sizeof(struct string21_struct));
	if (str == NULL)
		return NULL;
	alloc = byte_size_memory21(size);
	ptr = string21_malloc(alloc);
	if (ptr == NULL) {
		string21_free(str);
		return NULL;
	}
	if (clear_p)
		memset(ptr, 0, alloc);
	str->size = size;
	str->ptr = ptr;

	return str;
}

string21 *make_string21(size_t size)
{
	return make_string21_static(size, 0);
}

string21 *calloc_string21(size_t size)
{
	return make_string21_static(size, 1);
}

void free_string21(string21 *str)
{
	if (str) {
		string21_free(str->ptr);
		string21_free(str);
	}
}

void clear_string21(string21 *str)
{
	clear_memory21(str->ptr, str->size);
}

int getc_string21(const string21 *str, size_t i, unicode *ret)
{
	if (str->size <= i) {
		*ret = 0;
		return 1;
	}
	else {
		getc_memory21(str->ptr, i, ret);
		return 0;
	}
}

int putc_string21(string21 *str, size_t i, unicode c)
{
	if (str->size <= i)
		return 1;
	if (c < 0 || 0x110000 <= c)
		return 1;
	putc_memory21(str->ptr, i, c);
	return 0;
}

int compare_string21(const string21 *x, const string21 *y)
{
	size_t size_x, size_y;

	size_x = x->size;
	size_y = y->size;
	if (size_x < size_y)
		return -1;
	if (size_x > size_y)
		return 1;

	return compare_memory21(x->ptr, y->ptr, size_x);
}

int equal_string21(const string21 *x, const string21 *y)
{
	size_t size_x, size_y;

	size_x = x->size;
	size_y = y->size;
	if (size_x != size_y)
		return 0;

	return equal_memory21(x->ptr, y->ptr, size_x);
}

