#include <stdio.h>
#include <stdint.h>
#include "unicode.h"

int main(void)
{
	uint8_t array[4];
	int size, i, r;
	unicode c;
	struct state_utf8 x;

	/* Encode */
	encode_utf8(0x1F001, array, &size);
	for (i = 0; i < size; i++)
		printf("U+1F001 -> %X\n", array[i]);

	/* Decode */
	init_utf8(&x);
	for (i = 0; ; i++) {
		r = decode_utf8(&x, array[i], &c);
		if (r < 0) {
			printf("ERROR\n");
			return 1;
		}
		if (r)
			break;
	}
	printf("Decode: U+%X\n", c);

	return 0;
}

