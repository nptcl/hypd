/*
 *  Federal Information Processing Standards Publication 197
 *  November 26, 2001
 *  Announcing the ADVANCED ENCRYPTION STANDARD (AES)
 *  https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.197.pdf
 */
#include "aes.h"
#include <stdint.h>
#include <string.h>

/*
 *  init
 */
void init_aes128(struct aes *a)
{
	memset(a->key, 0, AES_KEY);
	a->bit = 128;
	a->byte = 128/8;
	a->nk = 4;
	a->nr = 10;
}

void init_aes192(struct aes *a)
{
	memset(a->key, 0, AES_KEY);
	a->bit = 192;
	a->byte = 192/8;
	a->nk = 6;
	a->nr = 12;
}

void init_aes256(struct aes *a)
{
	memset(a->key, 0, AES_KEY);
	a->bit = 256;
	a->byte = 256/8;
	a->nk = 8;
	a->nr = 14;
}


/*
 *  sub_bytes
 */
static const uint8_t aes_sub_byte1_table[] =
"\x63\x7c\x77\x7b\xf2\x6b\x6f\xc5\x30\x01\x67\x2b\xfe\xd7\xab\x76"
"\xca\x82\xc9\x7d\xfa\x59\x47\xf0\xad\xd4\xa2\xaf\x9c\xa4\x72\xc0"
"\xb7\xfd\x93\x26\x36\x3f\xf7\xcc\x34\xa5\xe5\xf1\x71\xd8\x31\x15"
"\x04\xc7\x23\xc3\x18\x96\x05\x9a\x07\x12\x80\xe2\xeb\x27\xb2\x75"
"\x09\x83\x2c\x1a\x1b\x6e\x5a\xa0\x52\x3b\xd6\xb3\x29\xe3\x2f\x84"
"\x53\xd1\x00\xed\x20\xfc\xb1\x5b\x6a\xcb\xbe\x39\x4a\x4c\x58\xcf"
"\xd0\xef\xaa\xfb\x43\x4d\x33\x85\x45\xf9\x02\x7f\x50\x3c\x9f\xa8"
"\x51\xa3\x40\x8f\x92\x9d\x38\xf5\xbc\xb6\xda\x21\x10\xff\xf3\xd2"
"\xcd\x0c\x13\xec\x5f\x97\x44\x17\xc4\xa7\x7e\x3d\x64\x5d\x19\x73"
"\x60\x81\x4f\xdc\x22\x2a\x90\x88\x46\xee\xb8\x14\xde\x5e\x0b\xdb"
"\xe0\x32\x3a\x0a\x49\x06\x24\x5c\xc2\xd3\xac\x62\x91\x95\xe4\x79"
"\xe7\xc8\x37\x6d\x8d\xd5\x4e\xa9\x6c\x56\xf4\xea\x65\x7a\xae\x08"
"\xba\x78\x25\x2e\x1c\xa6\xb4\xc6\xe8\xdd\x74\x1f\x4b\xbd\x8b\x8a"
"\x70\x3e\xb5\x66\x48\x03\xf6\x0e\x61\x35\x57\xb9\x86\xc1\x1d\x9e"
"\xe1\xf8\x98\x11\x69\xd9\x8e\x94\x9b\x1e\x87\xe9\xce\x55\x28\xdf"
"\x8c\xa1\x89\x0d\xbf\xe6\x42\x68\x41\x99\x2d\x0f\xb0\x54\xbb\x16";

static const uint8_t aes_sub_byte2_table[] =
"\x52\x09\x6a\xd5\x30\x36\xa5\x38\xbf\x40\xa3\x9e\x81\xf3\xd7\xfb"
"\x7c\xe3\x39\x82\x9b\x2f\xff\x87\x34\x8e\x43\x44\xc4\xde\xe9\xcb"
"\x54\x7b\x94\x32\xa6\xc2\x23\x3d\xee\x4c\x95\x0b\x42\xfa\xc3\x4e"
"\x08\x2e\xa1\x66\x28\xd9\x24\xb2\x76\x5b\xa2\x49\x6d\x8b\xd1\x25"
"\x72\xf8\xf6\x64\x86\x68\x98\x16\xd4\xa4\x5c\xcc\x5d\x65\xb6\x92"
"\x6c\x70\x48\x50\xfd\xed\xb9\xda\x5e\x15\x46\x57\xa7\x8d\x9d\x84"
"\x90\xd8\xab\x00\x8c\xbc\xd3\x0a\xf7\xe4\x58\x05\xb8\xb3\x45\x06"
"\xd0\x2c\x1e\x8f\xca\x3f\x0f\x02\xc1\xaf\xbd\x03\x01\x13\x8a\x6b"
"\x3a\x91\x11\x41\x4f\x67\xdc\xea\x97\xf2\xcf\xce\xf0\xb4\xe6\x73"
"\x96\xac\x74\x22\xe7\xad\x35\x85\xe2\xf9\x37\xe8\x1c\x75\xdf\x6e"
"\x47\xf1\x1a\x71\x1d\x29\xc5\x89\x6f\xb7\x62\x0e\xaa\x18\xbe\x1b"
"\xfc\x56\x3e\x4b\xc6\xd2\x79\x20\x9a\xdb\xc0\xfe\x78\xcd\x5a\xf4"
"\x1f\xdd\xa8\x33\x88\x07\xc7\x31\xb1\x12\x10\x59\x27\x80\xec\x5f"
"\x60\x51\x7f\xa9\x19\xb5\x4a\x0d\x2d\xe5\x7a\x9f\x93\xc9\x9c\xef"
"\xa0\xe0\x3b\x4d\xae\x2a\xf5\xb0\xc8\xeb\xbb\x3c\x83\x53\x99\x61"
"\x17\x2b\x04\x7e\xba\x77\xd6\x26\xe1\x69\x14\x63\x55\x21\x0c\x7d";

#define aes_table(table, x, y)		(table)[(x) * 16 + (y)]
#define aes_state(state, x, y)		(state)[(y) * 4 + (x)]

static void aes_sub_bytes0(struct aes *a, const uint8_t *table)
{
	int x, y;
	uint8_t *state, z, p, q;

	state = a->state;
	for (y = 0; y < 4; y++) {
		for (x = 0; x < 4; x++) {
			z = aes_state(state, x, y);
			p = z >> 4;
			q = z & 0x0F;
			aes_state(state, x, y) = aes_table(table, p, q);
		}
	}
}

static void aes_sub_bytes1(struct aes *a)
{
	aes_sub_bytes0(a, aes_sub_byte1_table);
}

static void aes_sub_bytes2(struct aes *a)
{
	aes_sub_bytes0(a, aes_sub_byte2_table);
}


/*
 *  shift_rows
 */
static void aes_shift_rows1(struct aes *a)
{
	uint8_t *state, x, y;

	/* 1 */
	state = a->state;
	x = aes_state(state, 1, 0);
	aes_state(state, 1, 0) = aes_state(state, 1, 1);
	aes_state(state, 1, 1) = aes_state(state, 1, 2);
	aes_state(state, 1, 2) = aes_state(state, 1, 3);
	aes_state(state, 1, 3) = x;

	/* 2 */
	x = aes_state(state, 2, 0);
	y = aes_state(state, 2, 1);
	aes_state(state, 2, 0) = aes_state(state, 2, 2);
	aes_state(state, 2, 1) = aes_state(state, 2, 3);
	aes_state(state, 2, 2) = x;
	aes_state(state, 2, 3) = y;

	/* 3 */
	x = aes_state(state, 3, 3);
	aes_state(state, 3, 3) = aes_state(state, 3, 2);
	aes_state(state, 3, 2) = aes_state(state, 3, 1);
	aes_state(state, 3, 1) = aes_state(state, 3, 0);
	aes_state(state, 3, 0) = x;
}

static void aes_shift_rows2(struct aes *a)
{
	uint8_t *state, x, y;

	/* 1 */
	state = a->state;
	x = aes_state(state, 3, 0);
	aes_state(state, 3, 0) = aes_state(state, 3, 1);
	aes_state(state, 3, 1) = aes_state(state, 3, 2);
	aes_state(state, 3, 2) = aes_state(state, 3, 3);
	aes_state(state, 3, 3) = x;

	/* 2 */
	x = aes_state(state, 2, 0);
	y = aes_state(state, 2, 1);
	aes_state(state, 2, 0) = aes_state(state, 2, 2);
	aes_state(state, 2, 1) = aes_state(state, 2, 3);
	aes_state(state, 2, 2) = x;
	aes_state(state, 2, 3) = y;

	/* 3 */
	x = aes_state(state, 1, 3);
	aes_state(state, 1, 3) = aes_state(state, 1, 2);
	aes_state(state, 1, 2) = aes_state(state, 1, 1);
	aes_state(state, 1, 1) = aes_state(state, 1, 0);
	aes_state(state, 1, 0) = x;
}


/*
 *  mix_columus
 */
#define aes_xtime(x) (0xFF & ((0x80 & (x))? (((x) << 1) ^ 0x1B): ((x) << 1)))

static uint8_t aes_mul_02(uint8_t x)
{
	return aes_xtime(x);
}

static uint8_t aes_mul_03(uint8_t x)
{
	return x ^ aes_xtime(x);
}

static void aes_mix_columns1(struct aes *a)
{
	int y;
	uint8_t *state, r1, r2, r3, r4;

	state = a->state;
	for (y = 0; y < 4; y++) {
		r1 = aes_state(state, 0, y);
		r2 = aes_state(state, 1, y);
		r3 = aes_state(state, 2, y);
		r4 = aes_state(state, 3, y);
		aes_state(state, 0, y) = aes_mul_02(r1) ^ aes_mul_03(r2) ^ r3 ^ r4;
		aes_state(state, 1, y) = r1 ^ aes_mul_02(r2) ^ aes_mul_03(r3) ^ r4;
		aes_state(state, 2, y) = r1 ^ r2 ^ aes_mul_02(r3) ^ aes_mul_03(r4);
		aes_state(state, 3, y) = aes_mul_03(r1) ^ r2 ^ r3 ^ aes_mul_02(r4);
	}
}

static uint8_t aes_mul_0e(uint8_t x)
{
	uint8_t y, z;

	/* 1110 */
	x = aes_xtime(x);
	y = aes_xtime(x);
	z = aes_xtime(y);
	return x ^ y ^ z;
}

static uint8_t aes_mul_0b(uint8_t x)
{
	uint8_t y, z;

	/* 1011 */
	y = aes_xtime(x);
	z = aes_xtime(y);
	z = aes_xtime(z);
	return x ^ y ^ z;
}

static uint8_t aes_mul_0d(uint8_t x)
{
	uint8_t y, z;

	/* 1101 */
	y = aes_xtime(x);
	y = aes_xtime(y);
	z = aes_xtime(y);
	return x ^ y ^ z;
}

static uint8_t aes_mul_09(uint8_t x)
{
	uint8_t y;

	/* 1001 */
	y = aes_xtime(x);
	y = aes_xtime(y);
	y = aes_xtime(y);
	return x ^ y;
}

#define aes_mul(n1,n2,n3,n4, r1,r2,r3,r4) \
	aes_mul_##n1(r1) ^ aes_mul_##n2(r2) ^ aes_mul_##n3(r3) ^ aes_mul_##n4(r4);

static void aes_mix_columns2(struct aes *a)
{
	int y;
	uint8_t *state, r1, r2, r3, r4;

	state = a->state;
	for (y = 0; y < 4; y++) {
		r1 = aes_state(state, 0, y);
		r2 = aes_state(state, 1, y);
		r3 = aes_state(state, 2, y);
		r4 = aes_state(state, 3, y);
		aes_state(state, 0, y) = aes_mul(0e,0b,0d,09, r1,r2,r3,r4);
		aes_state(state, 1, y) = aes_mul(09,0e,0b,0d, r1,r2,r3,r4);
		aes_state(state, 2, y) = aes_mul(0d,09,0e,0b, r1,r2,r3,r4);
		aes_state(state, 3, y) = aes_mul(0b,0d,09,0e, r1,r2,r3,r4);
	}
}


/*
 *  add_round_key
 */
static void aes_add_round_key(struct aes *a, int index)
{
	int y;
	uint8_t *state;
	uint32_t *word, v;

	state = a->state;
	word = a->word;
	index *= AES_NB;
	for (y = 0; y < 4; y++) {
		v = word[index + y];
		aes_state(state, 0, y) ^= (v >> 24) & 0xFF;
		aes_state(state, 1, y) ^= (v >> 16) & 0xFF;
		aes_state(state, 2, y) ^= (v >> 8) & 0xFF;
		aes_state(state, 3, y) ^= (v >> 0) & 0xFF;
	}
}


/*
 *  key_expansion
 */
#define aes_key_word(a,b,c,d) (((a) << 24UL) | ((b) << 16UL) | ((c) << 8UL) | (d))

static uint8_t aes_sub_word_byte(uint8_t x)
{
	int p, q;

	p = x >> 4;
	q = x & 0x0F;
	return aes_table(aes_sub_byte1_table, p, q);
}

static uint32_t aes_sub_word(uint32_t x)
{
	uint8_t *p;

	p = (uint8_t *)&x;
	p[0] = aes_sub_word_byte(p[0]);
	p[1] = aes_sub_word_byte(p[1]);
	p[2] = aes_sub_word_byte(p[2]);
	p[3] = aes_sub_word_byte(p[3]);

	return x;
}

static uint32_t aes_rot_word(uint32_t x)
{
	uint8_t a, b, c, d;

	a = 0xFFU & (x >> 24);
	b = 0xFFU & (x >> 16);
	c = 0xFFU & (x >> 8);
	d = 0xFFU & (x >> 0);
	return aes_key_word(b, c, d, a);
}

static uint32_t aes_rcon[] = {
	0x00000000, 0x01000000, 0x02000000, 0x04000000,
	0x08000000, 0x10000000, 0x20000000, 0x40000000,
	0x80000000, 0x1B000000, 0x36000000
};

void aes_key(struct aes *a)
{
	int i, i4, nk, nbnk1;
	uint8_t *key;
	uint32_t *word, temp;

	nk = a->nk;
	key = a->key;
	word = a->word;
	for (i = 0; i < nk; i++) {
		i4 = i * 4;
		word[i] = aes_key_word(key[i4 + 0], key[i4 + 1], key[i4 + 2], key[i4 + 3]);
	}

	nbnk1 = AES_NB * (a->nr + 1);
	for (i = nk; i < nbnk1; i++) {
		temp = word[i - 1];
		if ((i % nk) == 0)
			temp = aes_sub_word(aes_rot_word(temp)) ^ aes_rcon[i / nk];
		else if ((nk > 6) && ((i % nk) == 4))
			temp = aes_sub_word(temp);
		word[i] = word[i - nk] ^ temp;
	}
}


/*
 *  cipher
 */
void aes_cipher1(struct aes *a)
{
	int round;

	aes_add_round_key(a, 0);
	for (round = 1; round < a->nr; round++) {
		aes_sub_bytes1(a);
		aes_shift_rows1(a);
		aes_mix_columns1(a);
		aes_add_round_key(a, round);
	}
	aes_sub_bytes1(a);
	aes_shift_rows1(a);
	aes_add_round_key(a, a->nr);
}

void aes_cipher2(struct aes *a)
{
	int round;

	aes_add_round_key(a, a->nr);
	for (round = a->nr - 1; round; round--) {
		aes_shift_rows2(a);
		aes_sub_bytes2(a);
		aes_add_round_key(a, round);
		aes_mix_columns2(a);
	}
	aes_shift_rows2(a);
	aes_sub_bytes2(a);
	aes_add_round_key(a, 0);
}

