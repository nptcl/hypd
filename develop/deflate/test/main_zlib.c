#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <zlib.h>

#define CHUNK	0x010000

/*
 *  encode
 */
static int zlib_level(int op)
{
	switch (op) {
		case 0: return 0;
		case 1: return 1;
		case 2: return 6;
		case 3: return 9;
		default: return 6;
	}
}

static int zlib_encode_loop(z_stream *strm, FILE *fin, FILE *fout)
{
	int ret, flush, have;
	unsigned char in[CHUNK];
	unsigned char out[CHUNK];

	do {
		strm->avail_in = fread(in, 1, CHUNK, fin);
		if (ferror(fin)) {
			(void)deflateEnd(strm);
			return 1;
		}
		flush = feof(fin) ? Z_FINISH : Z_NO_FLUSH;
		strm->next_in = in;

		do {
			strm->avail_out = CHUNK;
			strm->next_out = out;
			ret = deflate(strm, flush);
			assert(ret != Z_STREAM_ERROR);
			have = CHUNK - strm->avail_out;
			if (fwrite(out, 1, have, fout) != have || ferror(fout)) {
				(void)deflateEnd(strm);
				return 1;
			}
		} while (strm->avail_out == 0);
		assert(strm->avail_in == 0);
	} while (flush != Z_FINISH);
	assert(ret == Z_STREAM_END);

	(void)deflateEnd(strm);
	return 0;
}

static int zlib_encode_call(int op, FILE *fin, FILE *fout)
{
	int level, ret;
	z_stream strm;

	level = zlib_level(op);
	strm.zalloc = Z_NULL;
	strm.zfree = Z_NULL;
	strm.opaque = Z_NULL;
	ret = deflateInit(&strm, level);
	if (ret != Z_OK)
		return 1;

	return zlib_encode_loop(&strm, fin, fout);
}

static int zlib_encode_call2(int op, FILE *fin, FILE *fout)
{
	int level, ret;
	z_stream strm;

	level = zlib_level(op);
	strm.zalloc = Z_NULL;
	strm.zfree = Z_NULL;
	strm.opaque = Z_NULL;
	ret = deflateInit2(&strm,
			level,
			Z_DEFLATED,
			15/*CINFO*/ + 16/*gzip*/,
			8/*default*/,
			Z_DEFAULT_STRATEGY);
	if (ret != Z_OK) {
		fprintf(stderr, "deflateInit2 error.\n");
		return 1;
	}

	return zlib_encode_loop(&strm, fin, fout);
}


/*
 *  decode
 */
static int zlib_decode_loop(z_stream *strm, FILE *fin, FILE *fout)
{
	int ret;
	unsigned have;
	unsigned char in[CHUNK];
	unsigned char out[CHUNK];

	do {
		strm->avail_in = fread(in, 1, CHUNK, fin);
		if (ferror(fin)) {
			(void)inflateEnd(strm);
			return 1;
		}
		if (strm->avail_in == 0)
			break;
		strm->next_in = in;

		do {
			strm->avail_out = CHUNK;
			strm->next_out = out;

			ret = inflate(strm, Z_NO_FLUSH);
			assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
			switch (ret) {
				case Z_NEED_DICT:
					ret = Z_DATA_ERROR;     /* and fall through */
				case Z_DATA_ERROR:
				case Z_MEM_ERROR:
					(void)inflateEnd(strm);
					return 1;
			}
			have = CHUNK - strm->avail_out;
			if (fwrite(out, 1, have, fout) != have || ferror(fout)) {
				(void)inflateEnd(strm);
				return 1;
			}
		} while (strm->avail_out == 0);
	} while (ret != Z_STREAM_END);

	/* clean up and return */
	(void)inflateEnd(strm);
	return ret == Z_STREAM_END ? 0 : 1;
}

static int zlib_decode_call(int ignore, FILE *fin, FILE *fout)
{
	int ret;
	z_stream strm;

	strm.zalloc = Z_NULL;
	strm.zfree = Z_NULL;
	strm.opaque = Z_NULL;
	strm.avail_in = 0;
	strm.next_in = Z_NULL;
	ret = inflateInit(&strm);
	if (ret != Z_OK)
		return 1;

	return zlib_decode_loop(&strm, fin, fout);
}

static int zlib_decode_call2(int ignore, FILE *fin, FILE *fout)
{
	int ret;
	z_stream strm;

	strm.zalloc = Z_NULL;
	strm.zfree = Z_NULL;
	strm.opaque = Z_NULL;
	strm.avail_in = 0;
	strm.next_in = Z_NULL;
	ret = inflateInit2(&strm, 15/*CINFO*/ + 16/*gzip*/);
	if (ret != Z_OK) {
		fprintf(stderr, "inflateInit2 error.\n");
		return 1;
	}

	return zlib_decode_loop(&strm, fin, fout);
}


/*
 *  main
 */
static int zlib_decode_FILE(int ignore, FILE *fin, FILE *fout)
{
	return zlib_decode_call(ignore, fin, fout);
}

static int zlib_encode_FILE(int op, FILE *fin, FILE *fout)
{
	return zlib_encode_call(op, fin, fout);
}

static int gzip_decode_FILE(int ignore, FILE *fin, FILE *fout)
{
	return zlib_decode_call2(ignore, fin, fout);
}

static int gzip_encode_FILE(int op, FILE *fin, FILE *fout)
{
	return zlib_encode_call2(op, fin, fout);
}

static int main_calltype(int i, int argc, char *argv[],
		int op, int (*call)(int op, FILE *fin, FILE *fout))
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
	check = (*call)(op, fin, fout);
	if (check)
		fprintf(stderr, "execute error.\n");
	if (close_output)
		fclose(fout);
	if (close_input)
		fclose(fin);
	return check? 1: 0;
}

static int main_zlib(int decode, int op, int i, int argc, char *argv[])
{
	if (decode)
		return main_calltype(i, argc, argv, op, zlib_decode_FILE);
	else
		return main_calltype(i, argc, argv, op, zlib_encode_FILE);
}

static int main_gzip(int decode, int op, int i, int argc, char *argv[])
{
	if (decode)
		return main_calltype(i, argc, argv, op, gzip_decode_FILE);
	else
		return main_calltype(i, argc, argv, op, gzip_encode_FILE);
}

static int main_help(char *argv[])
{
	printf("Usage: %s [type] [mode] ...\n", argv[0]);
	printf("\n");
	printf("[type]\n");
	printf("  -z:  zlib\n");
	printf("  -g:  gzip\n");
	printf("\n");
	printf("[mode]\n");
	printf("  -d:  decode\n");
	printf("  -e:  encode\n");
	printf("\n");

	return 1;
}

static int strequal(const char *x, const char *y)
{
	return strcmp(x, y) == 0;
}

int main(int argc, char *argv[])
{
	int i, decode, encode, option;
	const char *type, *mode;

	if (argc < 3)
		return main_help(argv);
	i = 1;
	type = argv[i++];
	mode = argv[i++];

	/* mode */
	option = 2;  /* default */
	if (strequal(mode, "-d")) { decode = 1; }
	if (strequal(mode, "-e")) { encode = 1; }
	if (strequal(mode, "-e0")) { encode = 1; option = 0; }
	if (strequal(mode, "-e1")) { encode = 1; option = 1; }
	if (strequal(mode, "-e2")) { encode = 1; option = 2; }
	if (strequal(mode, "-e3")) { encode = 1; option = 3; }
	if (decode == 0 && encode == 0)
		return main_help(argv);

	/* type */
	if (strequal(type, "-z"))
		return main_zlib(decode, option, i, argc, argv);
	if (strequal(type, "-g"))
		return main_gzip(decode, option, i, argc, argv);

	/* others */
	return main_help(argv);
}

