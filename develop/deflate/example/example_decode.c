#include "deflate_decode.h"
#include "example_decode.h"

/*
 *  FILE *example;
 */
#define InflateError(x) { \
	fprintf(stderr, "[ERROR] %s\n", x); \
}

#ifdef HYPD_DEBUG
#define INFLATE_FILE1  64
#define INFLATE_FILE2  64
#else
#define INFLATE_FILE1  4096
#define INFLATE_FILE2  4096
#endif

struct inflate_stdio {
	FILE *fin, *fout;
	char data1[INFLATE_FILE1];
	char data2[INFLATE_FILE2];
	struct inflate instance;
	const char *input;
	size_t size;
};

static int inflate_FILE_input(struct inflate_stdio *io)
{
	FILE *file;
	struct inflate *instance;
	size_t x;

	file = io->fin;
	if (io->size == 0) {
		io->size = fread(io->data1, 1, INFLATE_FILE1, file);
		if (io->size == 0) {
			if (ferror(file))
				return -1;
			if (feof(file))
				return 1;
		}
		io->input = io->data1;
	}
	instance = &(io->instance);
	io->input = inflate_input(instance, io->input, io->size, &x);
	io->size -= x;

	return 0;
}

static int inflate_FILE_output(struct inflate_stdio *io)
{
	FILE *file;
	char *output;
	struct inflate *instance;
	size_t size, x;

	file = io->fout;
	instance = &(io->instance);
	inflate_output(instance, io->data2, INFLATE_FILE2, &size);
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

static int inflate_FILE_execute(struct inflate_stdio *io)
{
	int check;
	struct inflate *instance;

	instance = &(io->instance);
	for (;;) {
		check = inflate_execute(instance);
		if (check < 0) {
			InflateError("decode error.");
			return -1;
		}
		if (check == 0)
			break;

		/* read */
		if (instance->input_call) {
			if (inflate_FILE_input(io)) {
				InflateError("input EOF error.");
				return -1;
			}
		}

		/* write */
		if (instance->output_call) {
			if (inflate_FILE_output(io)) {
				InflateError("output EOF error.");
				return -1;
			}
		}
	}

	return 0;
}

int inflate_FILE(FILE *fin, FILE *fout)
{
	struct inflate_stdio io;
	struct inflate *instance;

	instance = &(io.instance);
	inflate_init(instance);
	instance->flush = 1;
	io.input = NULL;
	io.size = 0;
	io.fin = fin;
	io.fout = fout;
	return inflate_FILE_execute(&io);
}

