/*
 *  [RFC 1951] DEFLATE Compressed Data Format Specification version 1.3
 */
#ifndef __DEFLATE_ENCODE_HEADER__
#define __DEFLATE_ENCODE_HEADER__

#include <stddef.h>
#include <stdint.h>

#ifndef DEFLATE_RING1
#ifdef HYPD_DEBUG
#define DEFLATE_RING1			16
#else
#define DEFLATE_RING1			4096
#endif
#endif

#ifndef DEFLATE_RING2
#ifdef HYPD_DEBUG
#define DEFLATE_RING2			32
#else
#define DEFLATE_RING2			4096
#endif
#endif

#define DEFLATE_PLANE			0x008000
#define DEFLATE_LZ77			0x008000
#define DEFLATE_WORK			0x010000

#define DEFLATE_HUFFMAN_CODE1	286
#define DEFLATE_HUFFMAN_CODE2	30
#define DEFLATE_HUFFMAN_CODE	(DEFLATE_HUFFMAN_CODE1 + DEFLATE_HUFFMAN_CODE2)

enum deflate_state {
	Deflate_start,
	Deflate_switch,
	Deflate_plane,
	Deflate_plane_loop,
	Deflate_plane_header,
	Deflate_plane_data,
	Deflate_flush,
	Deflate_final,
	Deflate_error
};

enum deflate_mode {
	deflate_mode_plane_only,
	deflate_mode_fixed_only,
	deflate_mode_dynamic_only,
	deflate_mode_fast,           /* gzip -1, -fast */
	deflate_mode_default,        /* gzip -6        */
	deflate_mode_slow            /* gzip -9, -best */
};

struct deflate_huffman {
	unsigned size : 6;
	unsigned code : 10;
	uint16_t huffman;
};

typedef void (*deflate_callback)(void *, uint8_t);

struct deflate {
	unsigned input_call : 1;
	unsigned output_call : 1;
	unsigned flush : 1;
	unsigned end_of_file : 1;
	unsigned lz77_disable : 1;
	uint8_t input[DEFLATE_RING1];
	uint8_t output[DEFLATE_RING2];
	uint8_t lz77[DEFLATE_LZ77];
	uint8_t work[DEFLATE_WORK];
	uint8_t outputv;
	unsigned state_index;
	unsigned inputa, inputb, inputc;
	unsigned outputa, outputb, outputc, outputx;
	unsigned lz77now, lz77size;
	unsigned work_now, work_size;
	enum deflate_mode mode;
	enum deflate_state state;
	deflate_callback input_callback;
	void *input_instance;
	struct deflate_huffman node1[DEFLATE_HUFFMAN_CODE1];
	struct deflate_huffman node2[DEFLATE_HUFFMAN_CODE2];
	struct deflate_huffman *node;
};


/*
 *  User Interface
 */
void deflate_init(struct deflate *encode);
const void *deflate_input(struct deflate *encode,
		const void *ptr, size_t size, size_t *ret);
void *deflate_output(struct deflate *encode,
		void *ptr, size_t size, size_t *ret);
int deflate_alignment(struct deflate *encode);
int deflate_execute(struct deflate *encode);
int deflate_break(struct deflate *encode);
int deflate_final(struct deflate *encode);
int deflate_restart(struct deflate *encode);


/*
 *  Module Interface
 */
int deflate_byte_write_p(struct deflate *encode, unsigned size);
int deflate_byte_putc(struct deflate *encode, uint8_t value);

#endif

