#include "deflate_gzip.h"
#include "example_gzip.h"

#define GzipError(x) { \
	fprintf(stderr, "[ERROR] %s\n", x); \
}

/*****************************************************************************
 *  FILE *decode;
 *****************************************************************************/
#ifdef HYPD_DEBUG
#define GZIP_DECODE_FILE1  64
#define GZIP_DECODE_FILE2  64
#else
#define GZIP_DECODE_FILE1  4096
#define GZIP_DECODE_FILE2  4096
#endif

struct gzip_decode_stdio {
	FILE *fin, *fout;
	char data1[GZIP_DECODE_FILE1];
	char data2[GZIP_DECODE_FILE2];
	struct gzip_decode instance;
	struct inflate *decode;
	const char *input;
	size_t size;
};

static int gzip_decode_FILE_input(struct gzip_decode_stdio *io)
{
	FILE *file;
	struct gzip_decode *instance;
	size_t x;

	file = io->fin;
	if (io->size == 0) {
		io->size = fread(io->data1, 1, GZIP_DECODE_FILE1, file);
		if (io->size == 0) {
			if (ferror(file))
				return -1;
			if (feof(file))
				return 1;
		}
		io->input = io->data1;
	}
	instance = &(io->instance);
	io->input = gzip_decode_input(instance, io->input, io->size, &x);
	io->size -= x;

	return 0;
}

static int gzip_decode_FILE_output(struct gzip_decode_stdio *io)
{
	FILE *file;
	char *output;
	struct gzip_decode *instance;
	size_t size, x;

	file = io->fout;
	instance = &(io->instance);
	gzip_decode_output(instance, io->data2, GZIP_DECODE_FILE2, &size);
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

static int gzip_decode_FILE_execute(struct gzip_decode_stdio *io)
{
	int check;
	struct gzip_decode *instance;
	struct inflate *decode;

	instance = &(io->instance);
	decode = io->decode;
	for (;;) {
		check = gzip_decode_execute(instance);
		if (check < 0) {
			GzipError("decode error.");
			return -1;
		}
		if (check == 0)
			break;

		/* read */
		if (decode->input_call) {
			if (gzip_decode_FILE_input(io)) {
				GzipError("input EOF error.");
				return -1;
			}
		}
		/* write */
		if (decode->output_call) {
			if (gzip_decode_FILE_output(io)) {
				GzipError("output EOF error.");
				return -1;
			}
		}
	}

	return 0;
}

static int gzip_decode_FILE_end(struct gzip_decode_stdio *io)
{
	int check;
	struct inflate *decode;

	/* inflate */
	decode = io->decode;
	if (decode->inputc != 0)
		return 1; /* exist */

	/* buffer */
	if (io->size)
		return 1; /* exist */

	/* stdio */
	check = gzip_decode_FILE_input(io);
	if (check < 0)
		return check; /* error */
	if (check)
		return 0; /* EOF */
	return 1; /* exist */
}

static int gzip_decode_FILE_member(struct gzip_decode_stdio *io)
{
	int check;
	struct gzip_decode *instance;


	instance = &(io->instance);
	for (;;) {
		/* gzip */
		check = gzip_decode_FILE_execute(io);
		if (check < 0) {
			GzipError("gzip_decode_FILE_execute error.");
			return -1;
		}
		if (check)
			return check;

		/* end */
		check = gzip_decode_FILE_end(io);
		if (check < 0) {
			GzipError("gzip_decode_FILE_end  error.");
			return -1;
		}
		if (check == 0)
			break;

		/* restart */
		if (gzip_decode_restart(instance)) {
			GzipError("gzip_decode_restart  error.");
			return -1;
		}
	}

	return 0;
}

int gzip_decode_FILE(FILE *fin, FILE *fout)
{
	struct gzip_decode_stdio io;
	struct gzip_decode *instance;

	instance = &(io.instance);
	gzip_decode_init(instance);
	io.decode = &(instance->decode);
	io.input = NULL;
	io.size = 0;
	io.fin = fin;
	io.fout = fout;
	return gzip_decode_FILE_member(&io);
}


/*****************************************************************************
 *  FILE *encode;
 *****************************************************************************/
#ifdef HYPD_DEBUG
#define GZIP_ENCODE_FILE1  64
#define GZIP_ENCODE_FILE2  64
#else
#define GZIP_ENCODE_FILE1  4096
#define GZIP_ENCODE_FILE2  4096
#endif

struct gzip_encode_stdio {
	FILE *fin, *fout;
	char data1[GZIP_ENCODE_FILE1];
	char data2[GZIP_ENCODE_FILE2];
	struct gzip_encode instance;
	struct deflate *encode;
	const char *input;
	size_t size;
};

static int gzip_encode_FILE_input(struct gzip_encode_stdio *io)
{
	FILE *file;
	struct gzip_encode *instance;
	size_t x;

	file = io->fin;
	if (io->size == 0) {
		io->size = fread(io->data1, 1, GZIP_ENCODE_FILE1, file);
		if (io->size == 0) {
			if (ferror(file))
				return -1;
			if (feof(file))
				return 1;
		}
		io->input = io->data1;
	}
	instance = &(io->instance);
	io->input = gzip_encode_input(instance, io->input, io->size, &x);
	io->size -= x;

	return 0;
}

static int gzip_encode_FILE_output(struct gzip_encode_stdio *io)
{
	FILE *file;
	char *output;
	struct gzip_encode *instance;
	size_t size, x;

	file = io->fout;
	instance = &(io->instance);
	gzip_encode_output(instance, io->data2, GZIP_ENCODE_FILE2, &size);
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

static int gzip_encode_FILE_break(struct gzip_encode_stdio *io)
{
	int check;
	struct gzip_encode *instance;

	/* input */
	check = gzip_encode_FILE_input(io);
	if (check < 0) {
		GzipError("deflate_FILE_input error.");
		return 1;
	}

	/* execute */
	if (check == 0)
		return 0;

	/* EOF */
	instance = &(io->instance);
	if (gzip_encode_break(instance)) {
		GzipError("gzip_encode_break error.");
		return 1;
	}

	return 0;
}

static int gzip_encode_FILE_execute(struct gzip_encode_stdio *io)
{
	int check;
	struct gzip_encode *instance;
	struct deflate *encode;

	instance = &(io->instance);
	encode = io->encode;
	for (;;) {
		check = gzip_encode_execute(instance);
		if (check < 0) {
			GzipError("encode error.");
			return -1;
		}
		if (check == 0)
			break;

		/* read */
		if (encode->input_call) {
			if (gzip_encode_FILE_break(io)) {
				GzipError("input EOF error.");
				return -1;
			}
		}
		/* write */
		if (encode->output_call) {
			if (gzip_encode_FILE_output(io)) {
				GzipError("output EOF error.");
				return -1;
			}
		}
	}

	return 0;
}

int gzip_encode_FILE(FILE *fin, FILE *fout)
{
	struct gzip_encode_stdio io;
	struct gzip_encode *instance;

	instance = &(io.instance);
	gzip_encode_init(instance);
	io.encode = &(instance->encode);
	io.input = NULL;
	io.size = 0;
	io.fin = fin;
	io.fout = fout;
	return gzip_encode_FILE_execute(&io);
}

