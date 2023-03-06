#include "deflate_encode.h"
#include "example_encode.h"

/*
 *  FILE *interface;
 */
#define DeflateError(x) { \
	fprintf(stderr, "[ERROR] %s\n", x); \
}

#ifdef HYPD_DEBUG
#define DEFLATE_FILE1  64
#define DEFLATE_FILE2  64
#else
#define DEFLATE_FILE1  4096
#define DEFLATE_FILE2  4096
#endif

struct deflate_stdio {
	FILE *fin, *fout;
	char data1[DEFLATE_FILE1];
	char data2[DEFLATE_FILE2];
	struct deflate instance;
	const char *input;
	size_t size;
};

static int deflate_FILE_input(struct deflate_stdio *io)
{
	FILE *file;
	struct deflate *instance;
	size_t x;

	file = io->fin;
	if (io->size == 0) {
		io->size = fread(io->data1, 1, DEFLATE_FILE1, file);
		if (io->size == 0) {
			if (ferror(file))
				return -1;
			if (feof(file))
				return 1;
		}
		io->input = io->data1;
	}
	instance = &(io->instance);
	io->input = deflate_input(instance, io->input, io->size, &x);
	io->size -= x;

	return 0;
}

static int deflate_FILE_output(struct deflate_stdio *io)
{
	FILE *file;
	char *output;
	struct deflate *instance;
	size_t size, x;

	file = io->fout;
	instance = &(io->instance);
	deflate_output(instance, io->data2, DEFLATE_FILE2, &size);
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

static int deflate_FILE_break(struct deflate_stdio *io)
{
	int check;
	struct deflate *instance;

	/* input */
	check = deflate_FILE_input(io);
	if (check < 0) {
		DeflateError("deflate_FILE_input error.");
		return 1;
	}

	/* execute */
	if (check == 0)
		return 0;

	/* EOF */
	instance = &(io->instance);
	if (deflate_break(instance)) {
		DeflateError("deflate_break error.");
		return 1;
	}

	return 0;
}

static int deflate_FILE_execute(struct deflate_stdio *io)
{
	int check;
	struct deflate *instance;

	instance = &(io->instance);
	for (;;) {
		check = deflate_execute(instance);
		if (check < 0) {
			DeflateError("encode error.");
			return -1;
		}
		if (check == 0)
			break;

		/* read */
		if (instance->input_call) {
			if (deflate_FILE_break(io)) {
				DeflateError("input EOF error.");
				return -1;
			}
		}

		/* write */
		if (instance->output_call) {
			if (deflate_FILE_output(io)) {
				DeflateError("output EOF error.");
				return -1;
			}
		}
	}

	return 0;
}

int deflate_FILE(FILE *fin, FILE *fout)
{
	struct deflate_stdio io;
	struct deflate *instance;

	instance = &(io.instance);
	deflate_init(instance);
	instance->flush = 1;
	io.input = NULL;
	io.size = 0;
	io.fin = fin;
	io.fout = fout;
	return deflate_FILE_execute(&io);
}

