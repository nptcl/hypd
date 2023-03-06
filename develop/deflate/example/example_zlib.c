#include "deflate_zlib.h"
#include "example_zlib.h"

#define ZlibError(x) { \
	fprintf(stderr, "[ERROR] %s\n", x); \
}

/*****************************************************************************
 *  FILE *decode;
 *****************************************************************************/
#ifdef HYPD_DEBUG
#define ZLIB_DECODE_FILE1  64
#define ZLIB_DECODE_FILE2  64
#else
#define ZLIB_DECODE_FILE1  4096
#define ZLIB_DECODE_FILE2  4096
#endif

struct zlib_decode_stdio {
	FILE *fin, *fout;
	char data1[ZLIB_DECODE_FILE1];
	char data2[ZLIB_DECODE_FILE2];
	struct zlib_decode instance;
	struct inflate *decode;
	const char *input;
	size_t size;
};

static int zlib_decode_FILE_input(struct zlib_decode_stdio *io)
{
	FILE *file;
	struct zlib_decode *instance;
	size_t x;

	file = io->fin;
	if (io->size == 0) {
		io->size = fread(io->data1, 1, ZLIB_DECODE_FILE1, file);
		if (io->size == 0) {
			if (ferror(file))
				return -1;
			if (feof(file))
				return 1;
		}
		io->input = io->data1;
	}
	instance = &(io->instance);
	io->input = zlib_decode_input(instance, io->input, io->size, &x);
	io->size -= x;

	return 0;
}

static int zlib_decode_FILE_output(struct zlib_decode_stdio *io)
{
	FILE *file;
	char *output;
	struct zlib_decode *instance;
	size_t size, x;

	file = io->fout;
	instance = &(io->instance);
	zlib_decode_output(instance, io->data2, ZLIB_DECODE_FILE2, &size);
	output = io->data2;
	while (size) {
		x = fwrite(output, 1, size, file);
		if (x == 0) {
			if (ferror(file))
				return -1;
			if (feof(file))
				return 1;
		}
		output += x;
		size -= x;
	}

	return 0;
}

static int zlib_decode_FILE_execute(struct zlib_decode_stdio *io)
{
	int check;
	struct zlib_decode *instance;
	struct inflate *decode;

	instance = &(io->instance);
	decode = io->decode;
	for (;;) {
		check = zlib_decode_execute(instance);
		if (check < 0) {
			ZlibError("decode error.");
			return -1;
		}
		if (check == 0)
			break;

		/* read */
		if (decode->input_call) {
			if (zlib_decode_FILE_input(io)) {
				ZlibError("input EOF error.");
				return -1;
			}
		}
		/* write */
		if (decode->output_call) {
			if (zlib_decode_FILE_output(io)) {
				ZlibError("output EOF error.");
				return -1;
			}
		}
	}

	return 0;
}

int zlib_decode_FILE(FILE *fin, FILE *fout)
{
	struct zlib_decode_stdio io;
	struct zlib_decode *instance;

	instance = &(io.instance);
	zlib_decode_init(instance);
	io.decode = &(instance->decode);
	io.input = NULL;
	io.size = 0;
	io.fin = fin;
	io.fout = fout;
	return zlib_decode_FILE_execute(&io);
}


/*****************************************************************************
 *  FILE *encode;
 *****************************************************************************/
#ifdef HYPD_DEBUG
#define ZLIB_ENCODE_FILE1  64
#define ZLIB_ENCODE_FILE2  64
#else
#define ZLIB_ENCODE_FILE1  4096
#define ZLIB_ENCODE_FILE2  4096
#endif

struct zlib_encode_stdio {
	FILE *fin, *fout;
	char data1[ZLIB_ENCODE_FILE1];
	char data2[ZLIB_ENCODE_FILE2];
	struct zlib_encode instance;
	struct deflate *encode;
	const char *input;
	size_t size;
};

static int zlib_encode_FILE_input(struct zlib_encode_stdio *io)
{
	FILE *file;
	struct zlib_encode *instance;
	size_t x;

	file = io->fin;
	if (io->size == 0) {
		io->size = fread(io->data1, 1, ZLIB_ENCODE_FILE1, file);
		if (io->size == 0) {
			if (ferror(file))
				return -1;
			if (feof(file))
				return 1;
		}
		io->input = io->data1;
	}
	instance = &(io->instance);
	io->input = zlib_encode_input(instance, io->input, io->size, &x);
	io->size -= x;

	return 0;
}

static int zlib_encode_FILE_output(struct zlib_encode_stdio *io)
{
	FILE *file;
	char *output;
	struct zlib_encode *instance;
	size_t size, x;

	file = io->fout;
	instance = &(io->instance);
	zlib_encode_output(instance, io->data2, ZLIB_ENCODE_FILE2, &size);
	output = io->data2;
	while (size) {
		x = fwrite(output, 1, size, file);
		if (x == 0) {
			if (ferror(file))
				return -1;
			if (feof(file))
				return 1;
		}
		output += x;
		size -= x;
	}

	return 0;
}

static int zlib_encode_FILE_break(struct zlib_encode_stdio *io)
{
	int check;
	struct zlib_encode *instance;

	/* input */
	check = zlib_encode_FILE_input(io);
	if (check < 0) {
		ZlibError("deflate_FILE_input error.");
		return 1;
	}

	/* execute */
	if (check == 0)
		return 0;

	/* EOF */
	instance = &(io->instance);
	if (zlib_encode_break(instance)) {
		ZlibError("zlib_encode_break error.");
		return 1;
	}

	return 0;
}

static int zlib_encode_FILE_execute(struct zlib_encode_stdio *io)
{
	int check;
	struct zlib_encode *instance;
	struct deflate *encode;

	instance = &(io->instance);
	encode = io->encode;
	for (;;) {
		check = zlib_encode_execute(instance);
		if (check < 0) {
			ZlibError("encode error.");
			return -1;
		}
		if (check == 0)
			break;

		/* read */
		if (encode->input_call) {
			if (zlib_encode_FILE_break(io)) {
				ZlibError("input EOF error.");
				return -1;
			}
		}
		/* write */
		if (encode->output_call) {
			if (zlib_encode_FILE_output(io)) {
				ZlibError("output EOF error.");
				return -1;
			}
		}
	}

	return 0;
}

int zlib_encode_FILE(FILE *fin, FILE *fout)
{
	struct zlib_encode_stdio io;
	struct zlib_encode *instance;

	instance = &(io.instance);
	zlib_encode_init(instance);
	io.encode = &(instance->encode);
	io.input = NULL;
	io.size = 0;
	io.fin = fin;
	io.fout = fout;
	return zlib_encode_FILE_execute(&io);
}

