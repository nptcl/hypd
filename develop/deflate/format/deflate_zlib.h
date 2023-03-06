/*
 *  [RFC 1950] ZLIB Compressed Data Format Specification version 3.3
 */
#ifndef __DEFLATE_ZLIB_HEADER__
#define __DEFLATE_ZLIB_HEADER__

#include "deflate_encode.h"
#include "deflate_decode.h"
#include <stdint.h>

struct zlib_adler32 {
	uint32_t s1, s2;
};

/*****************************************************************************
 *  decode
 *****************************************************************************/
enum zlib_decode_state {
	ZlibDecode_start,
	ZlibDecode_header,
	ZlibDecode_dictid,
	ZlibDecode_block,
	ZlibDecode_inflate,
	ZlibDecode_adler32,
	ZlibDecode_flush,
	ZlibDecode_final,
	ZlibDecode_failed
};

struct zlib_decode {
	unsigned flush : 1;
	unsigned header_cm : 4;
	unsigned header_cinfo : 4;
	unsigned header_fcheck : 5;
	unsigned header_fdict : 1;
	unsigned header_flevel : 2;
	enum zlib_decode_state state;
	unsigned state_index;
	struct zlib_adler32 adler32;
	uint32_t dictid;
	struct inflate decode;
};

void zlib_decode_init(struct zlib_decode *inst);
const void *zlib_decode_input(struct zlib_decode *inst,
		const void *ptr, size_t size, size_t *ret);
void *zlib_decode_output(struct zlib_decode *inst,
		void *ptr, size_t size, size_t *ret);
int zlib_decode_execute(struct zlib_decode *inst);
int zlib_decode_restart(struct zlib_decode *inst);


/*****************************************************************************
 *  encode
 *****************************************************************************/
enum zlib_encode_state {
	ZlibEncode_start,
	ZlibEncode_header,
	ZlibEncode_block,
	ZlibEncode_deflate,
	ZlibEncode_adler32,
	ZlibEncode_flush,
	ZlibEncode_final,
	ZlibEncode_failed
};

struct zlib_encode {
	unsigned flush : 1;
	unsigned header_fdict : 1;
	unsigned state_index;
	enum zlib_encode_state state;
	struct zlib_adler32 adler32;
	uint32_t dictid;
	struct deflate encode;
};

void zlib_encode_init(struct zlib_encode *inst);
const void *zlib_encode_input(struct zlib_encode *inst,
		const void *ptr, size_t size, size_t *ret);
void *zlib_encode_output(struct zlib_encode *inst,
		void *ptr, size_t size, size_t *ret);
int zlib_encode_execute(struct zlib_encode *inst);
int zlib_encode_break(struct zlib_encode *inst);
int zlib_encode_restart(struct zlib_encode *inst);

#endif

