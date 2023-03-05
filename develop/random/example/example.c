#include <stdio.h>
#include <string.h>
#include "md5encode.h"

int main(void)
{
	int i, k;
	uint8_t x[MD5ENCODE_SIZE];
	uint8_t a[2000];

	memset(a, 0, sizeof(a));
	for (k = 0; k < 2000; k++) {
		a[k] = k;
		sequence_md5encode(a, 2000, x);
		printf("%d: ", k);
		for (i = 0; i < MD5ENCODE_SIZE; i++)
			printf("%02x", x[i]);
		printf("\n");
	}

	return 0;
}

