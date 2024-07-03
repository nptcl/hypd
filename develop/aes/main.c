#include "aes.h"
#include <stdio.h>
#include <string.h>

void aes_encrypt(struct aes *a, FILE *input, FILE *output)
{
	for (;;) {
		memset(a->state, 0, AES_SIZE);
		if (fread(a->state, 1, AES_SIZE, input) == 0)
			break;
		aes_cipher1(a);
		fwrite(a->state, 1, AES_SIZE, output);
	}
}

void aes_decrypt(struct aes *a, FILE *input, FILE *output)
{
	for (;;) {
		memset(a->state, 0, AES_SIZE);
		if (fread(a->state, 1, AES_SIZE, input) == 0)
			break;
		aes_cipher2(a);
		fwrite(a->state, 1, AES_SIZE, output);
	}
}

int main(void)
{
	struct aes a;

	init_aes128(&a);
	memcpy(a.key, "Hello", 5);
	aes_key(&a);
	aes_encrypt(&a, stdin, stdout);

	return 0;
}

