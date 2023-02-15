/*
 *  md5encode.c
 *
 *  [RFC1321] The MD5 Message-Digest Algorithm
 *  https://www.ietf.org/rfc/rfc1321.txt
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "md5encode.h"

static const uint32_t Md5Encode_InitialWordA = 0x67452301;
static const uint32_t Md5Encode_InitialWordB = 0xEFCDAB89;
static const uint32_t Md5Encode_InitialWordC = 0x98BADCFE;
static const uint32_t Md5Encode_InitialWordD = 0x10325476;
static const uint32_t Md5Encode_CalcT[64 + 1] = {
	0x00000000,
	0xD76AA478, 0xE8C7B756, 0x242070DB, 0xC1BDCEEE,
	0xF57C0FAF, 0x4787C62A, 0xA8304613, 0xFD469501,
	0x698098D8, 0x8B44F7AF, 0xFFFF5BB1, 0x895CD7BE,
	0x6B901122, 0xFD987193, 0xA679438E, 0x49B40821,
	0xF61E2562, 0xC040B340, 0x265E5A51, 0xE9B6C7AA,
	0xD62F105D, 0x02441453, 0xD8A1E681, 0xE7D3FBC8,
	0x21E1CDE6, 0xC33707D6, 0xF4D50D87, 0x455A14ED,
	0xA9E3E905, 0xFCEFA3F8, 0x676F02D9, 0x8D2A4C8A,
	0xFFFA3942, 0x8771F681, 0x6D9D6122, 0xFDE5380C,
	0xA4BEEA44, 0x4BDECFA9, 0xF6BB4B60, 0xBEBFBC70,
	0x289B7EC6, 0xEAA127FA, 0xD4EF3085, 0x04881D05,
	0xD9D4D039, 0xE6DB99E5, 0x1FA27CF8, 0xC4AC5665,
	0xF4292244, 0x432AFF97, 0xAB9423A7, 0xFC93A039,
	0x655B59C3, 0x8F0CCC92, 0xFFEFF47D, 0x85845DD1,
	0x6FA87E4F, 0xFE2CE6E0, 0xA3014314, 0x4E0811A1,
	0xF7537E82, 0xBD3AF235, 0x2AD7D2BB, 0xEB86D391
};

#define Md5Encode_CalcF(x,y,z) (((x) & (y)) | ((~(x)) & (z)))
#define Md5Encode_CalcG(x,y,z) (((x) & (z)) | ((y) & (~(z))))
#define Md5Encode_CalcH(x,y,z) ((x) ^ (y) ^ (z))
#define Md5Encode_CalcI(x,y,z) ((y) ^ ((x) | (~(z))))
#define Md5Encode_CalcR32(v,s) (((v) << (s)) | ((v) >> (32 - (s))))
#define Md5Encode_Calc(op,a,b,c,d,k,s,i) { \
	a += Md5Encode_Calc##op(b,c,d) + x[k] + Md5Encode_CalcT[i]; \
	a = Md5Encode_CalcR32(a, s); \
	a += b; \
}

void clear_md5encode(struct md5encode *ptr)
{
	ptr->a = Md5Encode_InitialWordA;
	ptr->b = Md5Encode_InitialWordB;
	ptr->c = Md5Encode_InitialWordC;
	ptr->d = Md5Encode_InitialWordD;
	ptr->size = 0;
	ptr->pos = 0;
}

static void calcblock_md5encode(struct md5encode *ptr)
{
	uint32_t a, b, c, d, *x;

	a = ptr->a;
	b = ptr->b;
	c = ptr->c;
	d = ptr->d;
	x = ptr->x;

	/* Round 1. */
	Md5Encode_Calc(F, a,b,c,d,  0,  7,  1);
	Md5Encode_Calc(F, d,a,b,c,  1, 12,  2);
	Md5Encode_Calc(F, c,d,a,b,  2, 17,  3);
	Md5Encode_Calc(F, b,c,d,a,  3, 22,  4);
	Md5Encode_Calc(F, a,b,c,d,  4,  7,  5);
	Md5Encode_Calc(F, d,a,b,c,  5, 12,  6);
	Md5Encode_Calc(F, c,d,a,b,  6, 17,  7);
	Md5Encode_Calc(F, b,c,d,a,  7, 22,  8);
	Md5Encode_Calc(F, a,b,c,d,  8,  7,  9);
	Md5Encode_Calc(F, d,a,b,c,  9, 12, 10);
	Md5Encode_Calc(F, c,d,a,b, 10, 17, 11);
	Md5Encode_Calc(F, b,c,d,a, 11, 22, 12);
	Md5Encode_Calc(F, a,b,c,d, 12,  7, 13);
	Md5Encode_Calc(F, d,a,b,c, 13, 12, 14);
	Md5Encode_Calc(F, c,d,a,b, 14, 17, 15);
	Md5Encode_Calc(F, b,c,d,a, 15, 22, 16);

	/* Round 2. */
	Md5Encode_Calc(G, a,b,c,d,  1,  5, 17);
	Md5Encode_Calc(G, d,a,b,c,  6,  9, 18);
	Md5Encode_Calc(G, c,d,a,b, 11, 14, 19);
	Md5Encode_Calc(G, b,c,d,a,  0, 20, 20);
	Md5Encode_Calc(G, a,b,c,d,  5,  5, 21);
	Md5Encode_Calc(G, d,a,b,c, 10,  9, 22);
	Md5Encode_Calc(G, c,d,a,b, 15, 14, 23);
	Md5Encode_Calc(G, b,c,d,a,  4, 20, 24);
	Md5Encode_Calc(G, a,b,c,d,  9,  5, 25);
	Md5Encode_Calc(G, d,a,b,c, 14,  9, 26);
	Md5Encode_Calc(G, c,d,a,b,  3, 14, 27);
	Md5Encode_Calc(G, b,c,d,a,  8, 20, 28);
	Md5Encode_Calc(G, a,b,c,d, 13,  5, 29);
	Md5Encode_Calc(G, d,a,b,c,  2,  9, 30);
	Md5Encode_Calc(G, c,d,a,b,  7, 14, 31);
	Md5Encode_Calc(G, b,c,d,a, 12, 20, 32);

	/* Round 3. */
	Md5Encode_Calc(H, a,b,c,d,  5,  4, 33);
	Md5Encode_Calc(H, d,a,b,c,  8, 11, 34);
	Md5Encode_Calc(H, c,d,a,b, 11, 16, 35);
	Md5Encode_Calc(H, b,c,d,a, 14, 23, 36);
	Md5Encode_Calc(H, a,b,c,d,  1,  4, 37);
	Md5Encode_Calc(H, d,a,b,c,  4, 11, 38);
	Md5Encode_Calc(H, c,d,a,b,  7, 16, 39);
	Md5Encode_Calc(H, b,c,d,a, 10, 23, 40);
	Md5Encode_Calc(H, a,b,c,d, 13,  4, 41);
	Md5Encode_Calc(H, d,a,b,c,  0, 11, 42);
	Md5Encode_Calc(H, c,d,a,b,  3, 16, 43);
	Md5Encode_Calc(H, b,c,d,a,  6, 23, 44);
	Md5Encode_Calc(H, a,b,c,d,  9,  4, 45);
	Md5Encode_Calc(H, d,a,b,c, 12, 11, 46);
	Md5Encode_Calc(H, c,d,a,b, 15, 16, 47);
	Md5Encode_Calc(H, b,c,d,a,  2, 23, 48);

	/* Round 4. */
	Md5Encode_Calc(I, a,b,c,d,  0,  6, 49);
	Md5Encode_Calc(I, d,a,b,c,  7, 10, 50);
	Md5Encode_Calc(I, c,d,a,b, 14, 15, 51);
	Md5Encode_Calc(I, b,c,d,a,  5, 21, 52);
	Md5Encode_Calc(I, a,b,c,d, 12,  6, 53);
	Md5Encode_Calc(I, d,a,b,c,  3, 10, 54);
	Md5Encode_Calc(I, c,d,a,b, 10, 15, 55);
	Md5Encode_Calc(I, b,c,d,a,  1, 21, 56);
	Md5Encode_Calc(I, a,b,c,d,  8,  6, 57);
	Md5Encode_Calc(I, d,a,b,c, 15, 10, 58);
	Md5Encode_Calc(I, c,d,a,b,  6, 15, 59);
	Md5Encode_Calc(I, b,c,d,a, 13, 21, 60);
	Md5Encode_Calc(I, a,b,c,d,  4,  6, 61);
	Md5Encode_Calc(I, d,a,b,c, 11, 10, 62);
	Md5Encode_Calc(I, c,d,a,b,  2, 15, 63);
	Md5Encode_Calc(I, b,c,d,a,  9, 21, 64);

	ptr->a += a;
	ptr->b += b;
	ptr->c += c;
	ptr->d += d;
}

void read_md5encode(struct md5encode *ptr, const void *from, size_t len)
{
	int pos, j, k;
	size_t i;
	uint32_t *x;
	const uint8_t *byte;

	pos = ptr->pos;
	if (pos < 0) {
		fprintf(stderr, "md5encode is already finished.\n");
		abort();
	}
	x = ptr->x;
	j = pos / 4;
	k = pos % 4;
	byte = (const uint8_t *)from;
	for (i = 0;  i < len; i++) {
		if (64 <= pos) {
			calcblock_md5encode(ptr);
			pos = 0;
			j = k = 0;
		}
		if (k == 0) x[j] = 0;
		x[j] |= byte[i] << (8 * k);
		k++;
		if (4 <= k) {
			j++; k = 0;
		}
		pos++;
	}
	ptr->size += len;
	ptr->pos = pos;
}

static void wordtobyte_md5encode(uint32_t value, uint8_t *result)
{
	int i;

	for (i = 0; i < 4; i++) {
		result[i] = 0xFF & value;
		value >>= 8;
	}
}

static void calcfinal_md5encode(struct md5encode *ptr)
{
	size_t size, len, pos, k;
	uint8_t padding[64 + 8];

	/* padding */
	size = ptr->size;
	len = 64 - ((size + 8) % 64);
	if (len != 0)
		padding[0] = 0x80;
	for (pos = 1; pos < len; pos++)
		padding[pos] = 0;
	padding[pos++] = 0xFF & (size << 3);
	size >>= (8 - 3);
	for (k = 1; k < 8; k++, pos++, size >>= 8)
		padding[pos] = 0xFF & size;

	/* read padding */
	read_md5encode(ptr, padding, pos);
	calcblock_md5encode(ptr);
}

void calc_md5encode(struct md5encode *ptr, void *result)
{
	uint8_t *byte;

	if (0 <= ptr->pos) {
		calcfinal_md5encode(ptr);
		ptr->pos = -1;
	}

	byte = (uint8_t *)result;
	wordtobyte_md5encode(ptr->a, byte);
	wordtobyte_md5encode(ptr->b, byte + 4);
	wordtobyte_md5encode(ptr->c, byte + 8);
	wordtobyte_md5encode(ptr->d, byte + 12);
}

void sequence_md5encode(const void *from, size_t len, void *result)
{
	struct md5encode md5;

	clear_md5encode(&md5);
	read_md5encode(&md5, from, len);
	calc_md5encode(&md5, result);
}

void string_md5encode(const char *from, void *result)
{
	sequence_md5encode(from, strlen(from), result);
}

