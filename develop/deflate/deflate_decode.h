/*
 *  [RFC 1951] DEFLATE Compressed Data Format Specification version 1.3
 */
#ifndef __DEFLATE_DECODE_HEADER__
#define __DEFLATE_DECODE_HEADER__

#include <stddef.h>
#include <stdint.h>

#ifndef INFLATE_RING1
#ifdef HYPD_DEBUG
#define INFLATE_RING1			16
#else
#define INFLATE_RING1			4096
#endif
#endif

#ifndef INFLATE_RING2
#ifdef HYPD_DEBUG
#define INFLATE_RING2			32
#else
#define INFLATE_RING2			4096
#endif
#endif

#define INFLATE_LZ77			0x8000

#define INFLATE_HUFFMAN_CODE1	286
#define INFLATE_HUFFMAN_CODE2	30
#define INFLATE_HUFFMAN_CODE	(INFLATE_HUFFMAN_CODE1 + INFLATE_HUFFMAN_CODE2)

/* 1+2+4+8+...+512 + 15 + 1 */
#define INFLATE_HUFFMAN_SIZE1	(1023 + 15 + 1)
/* 1+2+4+8+...+64+ 15 + 1 */
#define INFLATE_HUFFMAN_SIZE2	(127 + 15 + 1)
/* plus */
#define INFLATE_HUFFMAN_SIZE	(INFLATE_HUFFMAN_SIZE1 + INFLATE_HUFFMAN_SIZE2)

enum inflate_state {
	Inflate_start,
	Inflate_header,
	Inflate_plane,
	Inflate_plane_pipe,
	Inflate_fixed,
	Inflate_dynamic,
	Inflate_dynamic_header,
	Inflate_dynamic_length,
	Inflate_dynamic_value,
	Inflate_decode,
	Inflate_flush,
	Inflate_final,
	Inflate_failed
};

struct inflate_node {
	unsigned link0 : 1;
	unsigned term0 : 1;
	unsigned value0 : 14;
	unsigned link1 : 1;
	unsigned term1 : 1;
	unsigned value1 : 14;
};

struct inflate_huffman {
	struct inflate_node *node;
	unsigned root, now, size;
};

typedef void (*inflate_callback)(void *, uint8_t);

struct inflate {
	unsigned input_call : 1;
	unsigned output_call : 1;
	unsigned finalbit : 1;
	unsigned flush : 1;
	uint8_t input[INFLATE_RING1];
	uint8_t output[INFLATE_RING2];
	uint8_t lz77[INFLATE_LZ77];
	uint8_t count[INFLATE_HUFFMAN_CODE];
	uint8_t dynamic_hdist, dynamic_hclen;
	uint16_t dynamic_hlit, dynamic_size;
	unsigned state_index;
	unsigned inputa, inputb, inputc, inputx;
	unsigned outputa, outputb, outputc;
	unsigned lz77now, lz77size, loop_size, loop_pos, loop_now;
	unsigned plane_size;
	unsigned decode_pos, decode_value;
	enum inflate_state state;
	inflate_callback output_callback;
	void *output_instance;
	struct inflate_node node1[INFLATE_HUFFMAN_SIZE1];
	struct inflate_node node2[INFLATE_HUFFMAN_SIZE2];
	struct inflate_huffman huffman1;
	struct inflate_huffman huffman2;
};


/*
 *  User Interface
 */
void inflate_init(struct inflate *decode);
const void *inflate_input(struct inflate *decode,
		const void *ptr, size_t size, size_t *ret);
void *inflate_output(struct inflate *decode,
		void *ptr, size_t size, size_t *ret);
int inflate_alignment(struct inflate *decode);
int inflate_execute(struct inflate *decode);
int inflate_restart(struct inflate *decode);


/*
 *  Module Interface
 */
int inflate_byte_read_p(struct inflate *decode, unsigned size);
int inflate_byte_getc(struct inflate *decode, uint8_t *ret);

#endif

