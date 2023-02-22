#include <stdio.h>
#include <string.h>
#include "deflate_decode.h"
#include "deflate_encode.h"
#include "example_decode.h"
#include "example_encode.h"
#include "example_gzip.h"
#include "example_zlib.h"

static int main_calltype(int i, int argc, char *argv[],
		int (*call)(FILE *fin, FILE *fout))
{
	int close_input, close_output, check;
	const char *name;
	FILE *fin, *fout;

	/* input */
	if (argc <= i) {
		fin = stdin;
		close_input = 0;
	}
	else {
		name = argv[i++];
		fin = fopen(name, "rb");
		if (fin == NULL) {
			fprintf(stderr, "Cannot open file, %s.\n", name);
			return 1;
		}
		close_input = 1;
	}

	/* output */
	if (argc <= i) {
		fout = stdout;
		close_output = 0;
	}
	else {
		name = argv[i++];
		fout = fopen(name, "wb");
		if (fout == NULL) {
			fprintf(stderr, "Cannot open file, %s.\n", name);
			if (close_input)
				fclose(fin);
			return 1;
		}
		close_output = 1;
	}

	/* execute */
	check = (*call)(fin, fout);
	if (check)
		fprintf(stderr, "execute error.\n");
	if (close_output)
		fclose(fout);
	if (close_input)
		fclose(fin);
	return check? 1: 0;
}

static int main_deflate(int decode, int i, int argc, char *argv[])
{
	if (decode)
		return main_calltype(i, argc, argv, inflate_FILE);
	else
		return main_calltype(i, argc, argv, deflate_FILE);
}

static int main_zlib(int decode, int i, int argc, char *argv[])
{
	if (decode)
		return main_calltype(i, argc, argv, zlib_decode_FILE);
	else
		return main_calltype(i, argc, argv, zlib_encode_FILE);
}

static int main_gzip(int decode, int i, int argc, char *argv[])
{
	if (decode)
		return main_calltype(i, argc, argv, gzip_decode_FILE);
	else
		return main_calltype(i, argc, argv, gzip_encode_FILE);
}

static int main_help(char *argv[])
{
	printf("Usage: %s [type] [mode] ...\n", argv[0]);
	printf("\n");
	printf("[type]\n");
	printf("  -r:  deflate\n");
	printf("  -z:  zlib\n");
	printf("  -g:  gzip\n");
	printf("\n");
	printf("[mode]\n");
	printf("  -d:  decode\n");
	printf("  -e:  encode  (Incomplete)\n");
	printf("\n");

	return 1;
}

static int main_debug(void)
{
	int size;
	unsigned x;

	/* decode */
	x = sizeof(struct inflate);
	printf("sizeof(struct inflate) = %u\n", x);
	x = sizeof(struct inflate_node [INFLATE_HUFFMAN_SIZE1]);
	size = INFLATE_HUFFMAN_SIZE1;
	printf("sizeof(struct inflate_node [%d]) = %u\n", size, x);
	x = sizeof(struct inflate_node [INFLATE_HUFFMAN_SIZE2]);
	size = INFLATE_HUFFMAN_SIZE2;
	printf("sizeof(struct inflate_node [%d]) = %u\n", size, x);
	x = sizeof(struct inflate_node [INFLATE_HUFFMAN_SIZE]);
	size = INFLATE_HUFFMAN_SIZE;
	printf("sizeof(struct inflate_node [%d]) = %u\n", size, x);
	x = sizeof(struct inflate_node);
	printf("sizeof(struct inflate_node) = %u\n", x);

	/* encode */
	x = sizeof(struct deflate);
	printf("sizeof(struct deflate) = %u\n", x);

	return 0;
}

static int strequal(const char *x, const char *y)
{
	return strcmp(x, y) == 0;
}

int main(int argc, char *argv[])
{
	int i, decode, encode;
	const char *type, *mode;

	if (argc == 2 && strequal(argv[1], "-debug"))
		return main_debug();

	if (argc < 3)
		return main_help(argv);
	i = 1;
	type = argv[i++];
	mode = argv[i++];

	/* mode */
	decode = strequal(mode, "-d");
	encode = strequal(mode, "-e");
	if (decode == 0 && encode == 0)
		return main_help(argv);

	/* type */
	if (strequal(type, "-r"))
		return main_deflate(decode, i, argc, argv);
	if (strequal(type, "-z"))
		return main_zlib(decode, i, argc, argv);
	if (strequal(type, "-g"))
		return main_gzip(decode, i, argc, argv);

	/* others */
	return main_help(argv);
}

