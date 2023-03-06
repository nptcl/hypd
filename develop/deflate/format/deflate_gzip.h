/*
 *  [RFC 1952] GZIP file format specification version 4.3
 */
#ifndef __DEFLATE_GZIP_HEADER__
#define __DEFLATE_GZIP_HEADER__

#include <stdint.h>
#include "deflate_decode.h"
#include "deflate_encode.h"

/*****************************************************************************
 *  decode
 *****************************************************************************/
enum gzip_decode_state {
	GzipDecode_start,
	GzipDecode_header,
	GzipDecode_extra,
	GzipDecode_extra_loop,
	GzipDecode_name,
	GzipDecode_comment,
	GzipDecode_crc16,
	GzipDecode_block,
	GzipDecode_inflate,
	GzipDecode_crc32,
	GzipDecode_size,
	GzipDecode_flush,
	GzipDecode_final,
	GzipDecode_failed
};

struct gzip_decode {
	unsigned flush : 1;
	unsigned header_ftext : 1;
	unsigned header_fhcrc : 1;
	unsigned header_fextra : 1;
	unsigned header_fname : 1;
	unsigned header_fcomment : 1;
	uint8_t header_xfl;
	uint8_t header_os;
	uint32_t header_mtime;
	uint32_t crc32;
	enum gzip_decode_state state;
	unsigned state_index;
	unsigned loop_size;
	size_t input_size;
	struct inflate decode;
};

void gzip_decode_init(struct gzip_decode *inst);
const void *gzip_decode_input(struct gzip_decode *inst,
		const void *ptr, size_t size, size_t *ret);
void *gzip_decode_output(struct gzip_decode *inst,
		void *ptr, size_t size, size_t *ret);
int gzip_decode_execute(struct gzip_decode *inst);
int gzip_decode_restart(struct gzip_decode *inst);


/*****************************************************************************
 *  encode
 *****************************************************************************/
enum gzip_encode_state {
	GzipEncode_start,
	GzipEncode_header,
	GzipEncode_crc16,
	GzipEncode_block,
	GzipEncode_deflate,
	GzipEncode_crc32,
	GzipEncode_size,
	GzipEncode_flush,
	GzipEncode_final,
	GzipEncode_failed
};

struct gzip_encode {
	unsigned flush : 1;
	unsigned header_ftext : 1;
	unsigned header_fhcrc : 1;
	uint8_t header_xfl;
	uint8_t header_os;
	uint32_t header_mtime;
	unsigned state_index;
	enum gzip_encode_state state;
	uint32_t crc32;
	size_t output_size;
	struct deflate encode;
};

void gzip_encode_init(struct gzip_encode *inst);
const void *gzip_encode_input(struct gzip_encode *inst,
		const void *ptr, size_t size, size_t *ret);
void *gzip_encode_output(struct gzip_encode *inst,
		void *ptr, size_t size, size_t *ret);
int gzip_encode_execute(struct gzip_encode *inst);
int gzip_encode_break(struct gzip_encode *inst);
int gzip_encode_restart(struct gzip_encode *inst);

#endif

