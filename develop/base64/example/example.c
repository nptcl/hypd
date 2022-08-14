#include "base64.h"
#include <stdio.h>

/***********************************************************************
 *  encode/decode example
 ***********************************************************************/
int main_encode(const char *str)
{
	char c, x, y;
	int i;
	struct base64_encode encode;

	/* initialize */
	base64_encode_init(&encode);

	/* pipe */
	for (i = 0; ; i++) {
		c = str[i];
		if (c == 0)
			break;
		base64_encode_pipe(&encode, (uint8_t)c, &x, &y);
		if (x)
			printf("%c", x);
		if (y)
			printf("%c", y);
	}

	/* close */
	for (;;) {
		base64_encode_closing(&encode, &x);
		if (x == 0)
			break;
		printf("%c", x);
	}
	printf("\n");

	return 0;
}

int main_decode(const char *str)
{
	char c;
	uint8_t x;
	int i, check;
	struct base64_decode decode;

	/* initialize */
	base64_decode_init(&decode);

	/* pipe */
	for (i = 0; ; i++) {
		c = str[i];
		if (c == 0)
			break;
		check = base64_decode_pipe(&decode, c, &x);
		if (check < 0) {
			fprintf(stderr, "decode error\n");
			return 1;
		}
		if (check)
			printf("%c", (int)x);
	}

	/* close */
	if (base64_decode_close(&decode)) {
		fprintf(stderr, "decode closing error\n");
		return 1;
	}
	printf("\n");

	return 0;
}

int main(void)
{
	const char *encode = "Hello";
	const char *decode = "SGVsbG8=";

	if (main_encode(encode))
		return 1;
	if (main_decode(decode))
		return 1;

	return 0;
}


/***********************************************************************
 *  pipe example
 ***********************************************************************/
static void encode_pipe_fputc(FILE *fout, int c, int i)
{
	fprintf(fout, "%c", c);
	if (i && (i % 80) == 0)
		fprintf(fout, "\n");
}

int encode_pipe(FILE *fin, FILE *fout)
{
	char x, y;
	int c, i;
	struct base64_encode encode;

	/* initialize */
	base64_encode_init(&encode);

	/* pipe */
	i = 0;
	for (;;) {
		c = fgetc(fin);
		if (c == EOF)
			break;
		base64_encode_pipe(&encode, (uint8_t)c, &x, &y);
		if (x)
			encode_pipe_fputc(fout, x, i++);
		if (y)
			encode_pipe_fputc(fout, y, i++);
	}

	/* close */
	for (;;) {
		base64_encode_closing(&encode, &x);
		if (x == 0)
			break;
		encode_pipe_fputc(fout, x, i++);
	}
	printf("\n");

	return 0;
}

int decode_pipe(FILE *fin, FILE *fout)
{
	uint8_t x;
	int c, check;
	struct base64_decode decode;

	/* initialize */
	base64_decode_init(&decode);

	/* pipe */
	for (;;) {
		c = fgetc(fin);
		if (c == EOF)
			break;
		check = base64_decode_pipe(&decode, (char)c, &x);
		if (check < 0) {
			fprintf(stderr, "decode error\n");
			return 1;
		}
		if (check)
			fputc((int)x, fout);
	}

	/* close */
	if (base64_decode_close(&decode)) {
		fprintf(stderr, "decode closing error\n");
		return 1;
	}

	return 0;
}

