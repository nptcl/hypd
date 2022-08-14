/*
 *  [RFC 4648] The Base16, Base32, and Base64 Data Encodings
 */
#ifndef __BASE64_HEADER__
#define __BASE64_HEADER__

#include <stdint.h>

/*****************************************************************************
 *  Base64 encode
 *****************************************************************************/
struct base64_encode {
	char char_62, char_63, char_padding;
	uint8_t data;
	int state, padding;
};

void base64_encode_init(struct base64_encode *ptr);
void base64_encode_clear(struct base64_encode *ptr);
int base64_encode_pipe(struct base64_encode *ptr, uint8_t c, char *x, char *y);
int base64_encode_closing(struct base64_encode *ptr, char *ret);


/*****************************************************************************
 *  Base64 decode
 *****************************************************************************/
struct base64_decode {
	char char_62, char_63, char_padding;
	uint8_t data;
	int state, ignore_eol, ignore_others, ignore_padding, ignore_check;
};

void base64_decode_init(struct base64_decode *ptr);
void base64_decode_clear(struct base64_decode *ptr);
int base64_decode_pipe(struct base64_decode *ptr, char c, uint8_t *ret);
int base64_decode_close(struct base64_decode *ptr);

#endif

