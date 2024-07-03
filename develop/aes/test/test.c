#include <stdio.h>
#include <stdlib.h>
#include "test.h"

/***********************************************************************
 *  test function
 ***********************************************************************/
int test_count = 0;
int test_error = 0;
int test_switch = 0;
int test_position = 0;

int test_execute(int check, const char *name)
{
	test_count++;
	if (check) {
		if (test_switch) {
			printf(".");
			test_position++;
			if (FIXED_DEGRADE_WIDTH <= test_position) {
				printf("\n");
				test_position = 0;
			}
		}
		else {
			printf("[OK] %7d: %s\n", test_count, name);
		}
		return 0;
	}
	else {
		if (test_switch) {
			if (test_position != 0) {
				printf("\n");
				test_position = 0;
			}
		}
		printf("[ERROR] %7d: %s\n", test_count, name);
		test_error++;
		return 1;
	}
}

void test_abort(void)
{
	printf("\n");
	fflush(NULL);
	printf("*************\n");
	printf("**  ERROR  **\n");
	printf("*************\n");
	fflush(NULL);
	exit(1);
}


/***********************************************************************
 *  test_aes
 ***********************************************************************/
#include "aes.h"
#include "aes.c"

#if 0
static uint32_t r32(uint32_t v)
{
	uint32_t r;

	r = 0;
	r |= (0xFF & (v >> 24)) << 0;
	r |= (0xFF & (v >> 16)) << 8;
	r |= (0xFF & (v >> 8)) << 16;
	r |= (0xFF & (v >> 0)) << 24;

	return r;
}
#else
static uint32_t r32(uint32_t v)
{
	return v;
}
#endif

static int test_setkey_char(char x)
{
	if ('0' <= x && x <= '9')
		return x - '0';
	if ('a' <= x && x <= 'f')
		return x - 'a' + 10;
	if ('A' <= x && x <= 'F')
		return x - 'A' + 10;

	return 0;  /* error */
}

static void test_setkey(struct aes *a, const char *str)
{
	int i, k, x, y;

	memset(a->key, 0, 32);
	for (i = 0; ; i++) {
		k = i * 2;
		if (str[k] == 0)
			break;
		x = test_setkey_char(str[k]);
		y = test_setkey_char(str[k + 1]);
		a->key[i] = ((x & 0x0F) << 4) | (y & 0x0F);
	}
}

static int test_appendix_key_128(void)
{
	uint8_t key[32];
	uint32_t x, y;
	struct aes a;

	key[0] = 0x2b;
	key[1] = 0x7e;
	key[2] = 0x15;
	key[3] = 0x16;
	x = aes_key_word(key[0], key[1], key[2], key[3]);
	test(x == r32(0x2b7e1516), "appendix_key_128.1");

	x = r32(0x09cf4f3c);
	x = aes_rot_word(x);
	test(x == r32(0xcf4f3c09), "appendix_key_128.2");

	x = aes_sub_word(x);
	test(x == r32(0x8a84eb01), "appendix_key_128.3");

	y = aes_rcon[1];
	test(y == r32(0x01000000), "appendix_key_128.4");

	x ^= y;
	test(x == r32(0x8b84eb01), "appendix_key_128.5");

	init_aes128(&a);
	test_setkey(&a, "2b7e151628aed2a6abf7158809cf4f3c");
	aes_key(&a);
	test(a.word[0] == r32(0x2b7e1516), "appendix_key_128.6");
	test(a.word[1] == r32(0x28aed2a6), "appendix_key_128.7");
	test(a.word[2] == r32(0xabf71588), "appendix_key_128.8");
	test(a.word[3] == r32(0x09cf4f3c), "appendix_key_128.9");

	test(a.word[4] == r32(0xa0fafe17), "appendix_key_128.10");
	test(a.word[5] == r32(0x88542cb1), "appendix_key_128.11");
	test(a.word[6] == r32(0x23a33939), "appendix_key_128.12");
	test(a.word[7] == r32(0x2a6c7605), "appendix_key_128.13");

	test(a.word[40] == r32(0xd014f9a8), "appendix_key_128.14");
	test(a.word[41] == r32(0xc9ee2589), "appendix_key_128.15");
	test(a.word[42] == r32(0xe13f0cc8), "appendix_key_128.16");
	test(a.word[43] == r32(0xb6630ca6), "appendix_key_128.17");

	Return;
}

static int test_appendix_key_192(void)
{
	struct aes a;

	init_aes192(&a);
	test_setkey(&a, "8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b");
	aes_key(&a);
	test(a.word[0] == r32(0x8e73b0f7), "appendix_key_192.1");
	test(a.word[1] == r32(0xda0e6452), "appendix_key_192.2");
	test(a.word[2] == r32(0xc810f32b), "appendix_key_192.3");
	test(a.word[3] == r32(0x809079e5), "appendix_key_192.4");
	test(a.word[4] == r32(0x62f8ead2), "appendix_key_192.5");
	test(a.word[5] == r32(0x522c6b7b), "appendix_key_192.6");

	test(a.word[6] == r32(0xfe0c91f7), "appendix_key_192.7");
	test(a.word[7] == r32(0x2402f5a5), "appendix_key_192.8");
	test(a.word[8] == r32(0xec12068e), "appendix_key_192.9");
	test(a.word[9] == r32(0x6c827f6b), "appendix_key_192.10");
	test(a.word[10] == r32(0x0e7a95b9), "appendix_key_192.11");
	test(a.word[11] == r32(0x5c56fec2), "appendix_key_192.12");

	test(a.word[42] == r32(0x821f750a), "appendix_key_192.14");
	test(a.word[43] == r32(0xad07d753), "appendix_key_192.15");
	test(a.word[44] == r32(0xca400538), "appendix_key_192.16");
	test(a.word[45] == r32(0x8fcc5006), "appendix_key_192.17");
	test(a.word[46] == r32(0x282d166a), "appendix_key_192.18");
	test(a.word[47] == r32(0xbc3ce7b5), "appendix_key_192.19");

	test(a.word[48] == r32(0xe98ba06f), "appendix_key_192.20");
	test(a.word[49] == r32(0x448c773c), "appendix_key_192.21");
	test(a.word[50] == r32(0x8ecc7204), "appendix_key_192.22");
	test(a.word[51] == r32(0x01002202), "appendix_key_192.23");

	Return;
}

static int test_appendix_key_256(void)
{
	struct aes a;

	init_aes256(&a);
	test_setkey(&a,
			"603deb1015ca71be2b73aef0857d7781"
			"1f352c073b6108d72d9810a30914dff4");
	aes_key(&a);
	test(a.word[0] == r32(0x603deb10), "appendix_key_256.1");
	test(a.word[1] == r32(0x15ca71be), "appendix_key_256.2");
	test(a.word[2] == r32(0x2b73aef0), "appendix_key_256.3");
	test(a.word[3] == r32(0x857d7781), "appendix_key_256.4");
	test(a.word[4] == r32(0x1f352c07), "appendix_key_256.5");
	test(a.word[5] == r32(0x3b6108d7), "appendix_key_256.6");
	test(a.word[6] == r32(0x2d9810a3), "appendix_key_256.7");
	test(a.word[7] == r32(0x0914dff4), "appendix_key_256.8");

	test(a.word[8] == r32(0x9ba35411), "appendix_key_256.9");
	test(a.word[9] == r32(0x8e6925af), "appendix_key_256.10");
	test(a.word[10] == r32(0xa51a8b5f), "appendix_key_256.11");
	test(a.word[11] == r32(0x2067fcde), "appendix_key_256.12");
	test(a.word[12] == r32(0xa8b09c1a), "appendix_key_256.13");
	test(a.word[13] == r32(0x93d194cd), "appendix_key_256.14");
	test(a.word[14] == r32(0xbe49846e), "appendix_key_256.15");
	test(a.word[15] == r32(0xb75d5b9a), "appendix_key_256.16");

	test(a.word[48] == r32(0x749c47ab), "appendix_key_256.17");
	test(a.word[49] == r32(0x18501dda), "appendix_key_256.18");
	test(a.word[50] == r32(0xe2757e4f), "appendix_key_256.19");
	test(a.word[51] == r32(0x7401905a), "appendix_key_256.20");
	test(a.word[52] == r32(0xcafaaae3), "appendix_key_256.21");
	test(a.word[53] == r32(0xe4d59b34), "appendix_key_256.22");
	test(a.word[54] == r32(0x9adf6ace), "appendix_key_256.22");
	test(a.word[55] == r32(0xbd10190d), "appendix_key_256.22");

	test(a.word[56] == r32(0xfe4890d1), "appendix_key_256.23");
	test(a.word[57] == r32(0xe6188d0b), "appendix_key_256.24");
	test(a.word[58] == r32(0x046df344), "appendix_key_256.25");
	test(a.word[59] == r32(0x706c631e), "appendix_key_256.26");

	Return;
}

static void test_setstate(struct aes *a, const char *str)
{
	int i, k, x, y;

	memset(a->state, 0, 16);
	for (i = 0; ; i++) {
		k = i * 2;
		if (str[k] == 0)
			break;
		x = test_setkey_char(str[k]);
		y = test_setkey_char(str[k + 1]);
		a->state[i] = ((x & 0x0F) << 4) | (y & 0x0F);
	}
}

static int test_state(struct aes *a, const char *str)
{
	uint8_t r;
	int i, k, x, y;

	for (i = 0; i < 16; i++) {
		k = i * 2;
		if (str[k] == 0)
			return 0;
		x = test_setkey_char(str[k]);
		y = test_setkey_char(str[k + 1]);
		r = ((x & 0x0F) << 4) | (y & 0x0F);
		if (a->state[i] != r)
			return 0;
	}

	return 1;
}

void aes_println_state(struct aes *a)
{
	int i;

	for (i = 0; i < 16; i++)
		printf("%02x", a->state[i]);
	printf("\n");
}

static int test_cipher1_128(void)
{
	int round;
	struct aes a;

	init_aes128(&a);
	test_setstate(&a, "00112233445566778899aabbccddeeff");
	test_setkey(&a, "000102030405060708090a0b0c0d0e0f");
	aes_key(&a);

	/* round = 0 */
	round = 0;
	test(test_state(&a, "00112233445566778899aabbccddeeff"), "cipher1_128.1");
	aes_add_round_key(&a, round);
	/* round = 1 */
	round = 1;
	test(test_state(&a, "00102030405060708090a0b0c0d0e0f0"), "cipher1_128.2");
	aes_sub_bytes1(&a);
	test(test_state(&a, "63cab7040953d051cd60e0e7ba70e18c"), "cipher1_128.3");
	aes_shift_rows1(&a);
	test(test_state(&a, "6353e08c0960e104cd70b751bacad0e7"), "cipher1_128.4");
	aes_mix_columns1(&a);
	test(test_state(&a, "5f72641557f5bc92f7be3b291db9f91a"), "cipher1_128.5");
	aes_add_round_key(&a, round);
	/* round = 2 */
	round = 2;
	test(test_state(&a, "89d810e8855ace682d1843d8cb128fe4"), "cipher1_128.6");

	/* cipher1 */
	init_aes128(&a);
	test_setstate(&a, "00112233445566778899aabbccddeeff");
	test_setkey(&a, "000102030405060708090a0b0c0d0e0f");
	aes_key(&a);
	aes_cipher1(&a);
	test(test_state(&a, "69c4e0d86a7b0430d8cdb78070b4c55a"), "cipher1_128.7");

	Return;
}

static int test_cipher2_128(void)
{
	int round;
	struct aes a;

	init_aes128(&a);
	test_setstate(&a, "69c4e0d86a7b0430d8cdb78070b4c55a");
	test_setkey(&a, "000102030405060708090a0b0c0d0e0f");
	aes_key(&a);

	/* round = 10 */
	round = 10;
	test(test_state(&a, "69c4e0d86a7b0430d8cdb78070b4c55a"), "cipher2_128.1");
	aes_add_round_key(&a, round);
	/* round = 9 */
	round = 9;
	test(test_state(&a, "7ad5fda789ef4e272bca100b3d9ff59f"), "cipher2_128.2");
	aes_shift_rows2(&a);
	test(test_state(&a, "7a9f102789d5f50b2beffd9f3dca4ea7"), "cipher2_128.3");
	aes_sub_bytes2(&a);
	test(test_state(&a, "bd6e7c3df2b5779e0b61216e8b10b689"), "cipher2_128.4");
	aes_add_round_key(&a, round);
	/* round = 8 */
	round = 8;
	test(test_state(&a, "e9f74eec023020f61bf2ccf2353c21c7"), "cipher2_128.5");

	/* cipher2 */
	init_aes128(&a);
	test_setstate(&a, "69c4e0d86a7b0430d8cdb78070b4c55a");
	test_setkey(&a, "000102030405060708090a0b0c0d0e0f");
	aes_key(&a);
	aes_cipher2(&a);
	test(test_state(&a, "00112233445566778899aabbccddeeff"), "cipher2_128.6");

	Return;
}

static int test_cipher1_192(void)
{
	struct aes a;

	init_aes192(&a);
	test_setstate(&a, "00112233445566778899aabbccddeeff");
	test_setkey(&a, "000102030405060708090a0b0c0d0e0f1011121314151617");
	aes_key(&a);
	aes_cipher1(&a);
	test(test_state(&a, "dda97ca4864cdfe06eaf70a0ec0d7191"), "cipher1_192.1");

	Return;
}

static int test_cipher2_192(void)
{
	struct aes a;

	init_aes192(&a);
	test_setstate(&a, "dda97ca4864cdfe06eaf70a0ec0d7191");
	test_setkey(&a, "000102030405060708090a0b0c0d0e0f1011121314151617");
	aes_key(&a);
	aes_cipher2(&a);
	test(test_state(&a, "00112233445566778899aabbccddeeff"), "cipher2_192.1");

	Return;
}

static int test_cipher1_256(void)
{
	struct aes a;

	init_aes256(&a);
	test_setstate(&a, "00112233445566778899aabbccddeeff");
	test_setkey(&a, "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f");
	aes_key(&a);
	aes_cipher1(&a);
	test(test_state(&a, "8ea2b7ca516745bfeafc49904b496089"), "cipher1_256.1");

	Return;
}

static int test_cipher2_256(void)
{
	struct aes a;

	init_aes256(&a);
	test_setstate(&a, "8ea2b7ca516745bfeafc49904b496089");
	test_setkey(&a, "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f");
	aes_key(&a);
	aes_cipher2(&a);
	test(test_state(&a, "00112233445566778899aabbccddeeff"), "cipher2_256.1");

	Return;
}

static int test_aes(void)
{
	TestCall(test_appendix_key_128);
	TestCall(test_appendix_key_192);
	TestCall(test_appendix_key_256);
	TestCall(test_cipher1_128);
	TestCall(test_cipher2_128);
	TestCall(test_cipher1_192);
	TestCall(test_cipher2_192);
	TestCall(test_cipher1_256);
	TestCall(test_cipher2_256);

	return 0;
}


/***********************************************************************
 *  main
 ***********************************************************************/
int main(void)
{
	test_aes();
	if (test_error)
		test_abort();
	printf("OK: %d.\n", test_count);

	return 0;
}

