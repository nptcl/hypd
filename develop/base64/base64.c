#include "base64.h"
#include <stdio.h>
#include <stdint.h>

/*****************************************************************************
 *  Base64 encode
 *****************************************************************************/
static const char base64_encode_table[] = {
	'A','B','C','D','E','F','G','H','I','J','K','L','M',
	'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
	'a','b','c','d','e','f','g','h','i','j','k','l','m',
	'n','o','p','q','r','s','t','u','v','w','x','y','z',
	'0','1','2','3','4','5','6','7','8','9'
};

void base64_encode_init(struct base64_encode *ptr)
{
	ptr->padding = 1;
	ptr->state = 0;
	ptr->data = 0U;
	ptr->char_62 = '+';
	ptr->char_63 = '/';
	ptr->char_padding = '=';
}

void base64_encode_clear(struct base64_encode *ptr)
{
	ptr->state = 0;
	ptr->data = 0U;
}

static char base64_encode_value(struct base64_encode *ptr, uint8_t c)
{
	switch (c) {
		case 62U:
			return ptr->char_62;

		case 63U:
			return ptr->char_63;

		default:
			return base64_encode_table[c];
	}
}

int base64_encode_pipe(struct base64_encode *ptr, uint8_t c, char *x, char *y)
{
	uint8_t v;

	switch (ptr->state) {
		case 0:
			v = (c >> 2U);
			*x = base64_encode_value(ptr, v);
			*y = 0;
			ptr->data = (c & 0x03);
			ptr->state = 1;
			return 1;

		case 1:
			v = (ptr->data << 4U) | (c >> 4U);
			*x = base64_encode_value(ptr, v);
			*y = 0;
			ptr->data = (c & 0x0F);
			ptr->state = 2;
			return 1;

		case 2:
			v = (ptr->data << 2U) | (c >> 6U);
			*x = base64_encode_value(ptr, v);
			*y = base64_encode_value(ptr, (c & 0x3F));
			ptr->data = 0U;
			ptr->state = 0;
			return 2;

		default:
			*x = *y = 0;
			ptr->data = 0U;
			ptr->state = -1;
			return -1; /* error */
	}
}

int base64_encode_closing(struct base64_encode *ptr, char *ret)
{
	uint8_t v;

	switch (ptr->state) {
		case 0:
		case 5:
			*ret = 0;
			ptr->data = 0U;
			ptr->state = -1;
			return 0;

		case 1:
			v = (ptr->data << 4U);
			*ret = base64_encode_value(ptr, v);
			ptr->data = 0U;
			ptr->state = ptr->padding? 3: 5;
			return 1;

		case 2:
			v = (ptr->data << 2U);
			*ret = base64_encode_value(ptr, v);
			ptr->data = 0U;
			ptr->state = ptr->padding? 4: 5;
			return 1;

		case 3:
			*ret = ptr->char_padding;
			ptr->state = 4;
			return 1;

		case 4:
			*ret = ptr->char_padding;
			ptr->state = 5;
			return 1;

		default:
			*ret = 0;
			ptr->data = 0U;
			ptr->state = -1;
			return -1; /* error */
	}
}


/*****************************************************************************
 *  Base64 decode
 *****************************************************************************/
static const int base64_decode_table[] = {
	/*0*/ -1,-1,-1,-1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,-1,-1,-1,
	/*1*/ -1,-1,-1,-1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,-1,-1,-1,
	/*2*/ -1,-1,-1,-1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,-1,-1,-1,
	/*3*/ 52,53,54,55,56,57,58,59,   60,61,-1,-1,-1,-1,-1,-1,
	/*4*/ -1, 0, 1, 2, 3, 4, 5, 6,    7, 8, 9,10,11,12,13,14,
	/*5*/ 15,16,17,18,19,20,21,22,   23,24,25,-1,-1,-1,-1,-1,
	/*6*/ -1,26,27,28,29,30,31,32,   33,34,35,36,37,38,39,40,
	/*7*/ 41,42,43,44,45,46,47,48,   49,50,51,-1,-1,-1,-1,-1
};

void base64_decode_init(struct base64_decode *ptr)
{
	ptr->state = 0;
	ptr->data = 0U;
	ptr->ignore_eol = 1;
	ptr->ignore_others = 0;
	ptr->ignore_padding = 0;
	ptr->ignore_check = 1;
	ptr->char_62 = '+';
	ptr->char_63 = '/';
	ptr->char_padding = '=';
}

void base64_decode_clear(struct base64_decode *ptr)
{
	ptr->state = 0;
	ptr->data = 0U;
}

static int base64_decode_error(struct base64_decode *ptr, uint8_t *ret)
{
	if (ret)
		*ret = 0xFFU;
	ptr->data = 0U;
	ptr->state = -1;
	return -1;
}

static int base64_decode_value(struct base64_decode *ptr, uint8_t u, uint8_t *ret)
{
	switch (ptr->state) {
		case 0:
			*ret = 0xFFU;
			ptr->data = u;
			ptr->state = 1;
			return 0; /* no-data */

		case 1:
			*ret = (ptr->data << 2U) | (u >> 4U);
			ptr->data = (u & 0x0F);
			ptr->state = 2;
			return 1; /* 1-data */

		case 2:
			*ret = (ptr->data << 4U) | (u >> 2U);
			ptr->data = (u & 0x03);
			ptr->state = 3;
			return 1; /* 1-data */

		case 3:
			*ret = (ptr->data << 6U) | u;
			ptr->data = 0U;
			ptr->state = 0;
			return 1; /* 1-data */

		default:
			return base64_decode_error(ptr, ret);
	}
}

static int base64_decode_padding(struct base64_decode *ptr, uint8_t *ret)
{
	if (ptr->ignore_check == 0 && ptr->data)
		return base64_decode_error(ptr, ret);

	*ret = 0xFFU;
	ptr->data = 0U;
	switch (ptr->state) {
		case 0:
		case 7:
			ptr->state = 4;
			return 0;

		case 1:
		case 4:
			ptr->state = 5;
			return 0;

		case 2:
		case 5:
			ptr->state = 6;
			return 0;

		case 3:
		case 6:
			ptr->state = 7;
			return 0;

		default:
			return base64_decode_error(ptr, ret);
	}
}

static int base64_decode_check(struct base64_decode *ptr, int check, uint8_t *ret)
{
	if (check) {
		*ret = 0xFFU;
		return 0;
	}
	return base64_decode_error(ptr, ret);
}

static int base64_decode_get(char c)
{
	return ((c < 0) || (0x7F < c))?
		-1:
		base64_decode_table[(int)c];
}

int base64_decode_pipe(struct base64_decode *ptr, char c, uint8_t *ret)
{
	int v;

	/* 62, 63 */
	if (c == ptr->char_62)
		return base64_decode_value(ptr, 62U, ret);
	if (c == ptr->char_63)
		return base64_decode_value(ptr, 63U, ret);

	/* padding */
	if (c == ptr->char_padding)
		return base64_decode_padding(ptr, ret);

	/* eol */
	if (c == 0x0A || c == 0x0D)
		return base64_decode_check(ptr, ptr->ignore_eol, ret);

	/* others */
	v = base64_decode_get(c);
	if (v < 0)
		return base64_decode_check(ptr, ptr->ignore_others, ret);

	/* character */
	return base64_decode_value(ptr, (uint8_t)v, ret);
}

static int base64_decode_close_1(struct base64_decode *ptr)
{
	ptr->data = 0U;
	switch (ptr->state) {
		case 1:
			ptr->state = 5;
			return 0;

		case 2:
			ptr->state = 6;
			return 0;

		case 3:
			ptr->state = 7;
			return 0;

		case 0:
		case 4:
		case 5:
		case 6:
		case 7:
			ptr->state = -1;
			return 0; /* finish */

		default:
			return base64_decode_error(ptr, NULL);
	}
}

static int base64_decode_close_2(struct base64_decode *ptr)
{
	switch (ptr->state) {
		case 0:
		case 7:
			ptr->data = 0U;
			ptr->state = -1;
			return 0; /* finish */

		default:
			return base64_decode_error(ptr, NULL);
	}
}

int base64_decode_close(struct base64_decode *ptr)
{
	if (ptr->ignore_check == 0 && ptr->data)
		return base64_decode_error(ptr, NULL);
	if (ptr->ignore_padding)
		return base64_decode_close_1(ptr);
	else
		return base64_decode_close_2(ptr);
}

