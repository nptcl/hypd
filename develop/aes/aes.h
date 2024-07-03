#ifndef __AES_HEADER__
#define __AES_HEADER__

#include <stdint.h>

#define AES_SIZE		(4*4)
#define AES_KEY			32
#define AES_NB			4

struct aes {
	uint8_t key[AES_KEY];
	uint8_t state[AES_SIZE];
	uint32_t word[AES_NB*(14+1)];
	int nk, nr, bit, byte;
};

void init_aes128(struct aes *a);
void init_aes192(struct aes *a);
void init_aes256(struct aes *a);

void aes_key(struct aes *a);
void aes_cipher1(struct aes *a);
void aes_cipher2(struct aes *a);

#endif

