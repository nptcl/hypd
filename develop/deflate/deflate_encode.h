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
#define DEFLATE_CODE			0x010000

#ifdef HYPD_DEBUG
#define DEFLATE_HASH			100
#else
#define DEFLATE_HASH			0x010000
#endif

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
	Deflate_fixed,
	Deflate_fixed_header,
	Deflate_fixed_encode,
	Deflate_dynamic,
	Deflate_dynamic_loop,
	Deflate_dynamic_header,
	Deflate_dynamic_encode,
	Deflate_flush,
	Deflate_final,
	Deflate_failed
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
	unsigned size : 4;			/* 0...15 */
	unsigned code : 15;			/* 0...(expt 2 15) */
};

struct deflate_hash {
	unsigned enable : 1;
	uint8_t key[3];
	size_t position;
};

struct deflate_code {
	unsigned code_p : 1;		/* code or lz77 */
	unsigned code : 8;			/* 0 ... 255 */
	unsigned distance : 15;		/* 0...(expt 2 15) */
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
	uint8_t outputv, input1, input2;
	unsigned state_index;
	unsigned inputa, inputb, inputc;
	unsigned outputa, outputb, outputc, outputx;
	unsigned lz77now, lz77size;
	unsigned work_now, work_size;
	enum deflate_mode mode;
	enum deflate_state state;
	deflate_callback input_callback;
	void *input_instance;
	size_t position;
	struct deflate_huffman node1[DEFLATE_HUFFMAN_CODE1];
	struct deflate_huffman node2[DEFLATE_HUFFMAN_CODE2];
	struct deflate_hash hash[DEFLATE_HASH];
	struct deflate_code code[DEFLATE_CODE];
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
int deflate_restart(struct deflate *encode);


/*
 *  Module Interface
 */
int deflate_byte_write_p(struct deflate *encode, unsigned size);
int deflate_byte_putc(struct deflate *encode, uint8_t value);

#endif

