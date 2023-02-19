#ifndef __UNICODE_HEADER__
#define __UNICODE_HEADER__

#include <stdint.h>
#include <stdio.h>

#ifndef string21_malloc
#define string21_malloc malloc
#endif
#ifndef string21_free
#define string21_free free
#endif

typedef int32_t unicode;

#define UNICODE_COUNT		0x110000


/*
 *  UTF-8
 */
struct state_utf8 {
	int state, error;
	unicode value;
};

void init_utf8(struct state_utf8 *ptr);
int decode_utf8(struct state_utf8 *ptr, uint8_t c, unicode *ret);
int encode_utf8(unicode c, uint8_t *dst, int *ret);
int length_utf8(unicode c, unsigned *ret);


/*
 *  UTF-16
 */
struct state_utf16 {
	int state, error;
	unicode value;
};

void init_utf16(struct state_utf16 *ptr);
int decode_utf16(struct state_utf16 *ptr, uint16_t c, unicode *ret);
int encode_utf16(unicode c, uint16_t *dst, int *ret);
int length_utf16(unicode c, unsigned *ret);


/*
 *  UTF-16LE
 */
struct state_utf16le {
	struct state_utf16 decode;
	int state, error;
	uint8_t value;
};

void init_utf16le(struct state_utf16le *ptr);
int decode_utf16le(struct state_utf16le *ptr, uint8_t c, unicode *ret);
int encode_utf16le(unicode c, uint8_t *dst, int *ret);
int length_utf16le(unicode c, unsigned *ret);


/*
 *  UTF-16BE
 */
struct state_utf16be {
	struct state_utf16 decode;
	int state, error;
	uint8_t value;
};

void init_utf16be(struct state_utf16be *ptr);
int decode_utf16be(struct state_utf16be *ptr, uint8_t c, unicode *ret);
int encode_utf16be(unicode c, uint8_t *dst, int *ret);
int length_utf16be(unicode c, unsigned *ret);


/*
 *  UTF-32
 */
int length_utf32(unicode c, unsigned *ret);


/*
 *  UTF-32LE
 */
struct state_utf32le {
	int state, error;
	unicode value;
};

void init_utf32le(struct state_utf32le *ptr);
int decode_utf32le(struct state_utf32le *ptr, uint8_t c, unicode *ret);
int encode_utf32le(unicode c, uint8_t *dst, int *ret);
int length_utf32le(unicode c, unsigned *ret);


/*
 *  UTF-32BE
 */
struct state_utf32be {
	int state, error;
	unicode value;
};

void init_utf32be(struct state_utf32be *ptr);
int decode_utf32be(struct state_utf32be *ptr, uint8_t c, unicode *ret);
int encode_utf32be(unicode c, uint8_t *dst, int *ret);
int length_utf32be(unicode c, unsigned *ret);


/*
 *  Byte Order Mark, BOM
 */
#define BYTE_ORDER_MARK_HISTORY		8
#define BYTE_ORDER_MASK_VALUE		0xFEFF

enum byte_order_mark_type {
	byte_order_mark_next,
	byte_order_mark_UTF8,
	byte_order_mark_UTF16LE,
	byte_order_mark_UTF16BE,
	byte_order_mark_UTF32LE,
	byte_order_mark_UTF32BE,
	byte_order_mark_binary,
	byte_order_mark_error,
};

struct byte_order_mark {
	int state, error, index;
	enum byte_order_mark_type type;
	uint8_t data[BYTE_ORDER_MARK_HISTORY];
};

void init_byte_order_mark(struct byte_order_mark *ptr);
int getc_byte_order_mark(struct byte_order_mark *ptr, int i, uint8_t *ret);
enum byte_order_mark_type putc_byte_order_mark(struct byte_order_mark *ptr, uint8_t c);
enum byte_order_mark_type result_byte_order_mark(struct byte_order_mark *ptr);


/*
 *  stdio
 */
int fgetc_utf8(FILE *file, unicode *ret);
int fgetc_utf16le(FILE *file, unicode *ret);
int fgetc_utf16be(FILE *file, unicode *ret);
int fgetc_utf32le(FILE *file, unicode *ret);
int fgetc_utf32be(FILE *file, unicode *ret);

int fputc_utf8(unicode c, FILE *file);
int fputc_utf16le(unicode c, FILE *file);
int fputc_utf16be(unicode c, FILE *file);
int fputc_utf32le(unicode c, FILE *file);
int fputc_utf32be(unicode c, FILE *file);


/*
 *  memory21
 */
size_t byte_size_memory21(size_t unicode_size);
size_t unicode_size_memory21(size_t byte_size);
void clear_memory21(void *ptr, size_t unicode_size);
void getc_memory21(const void *ptr, size_t i, unicode *ret);
void putc_memory21(void *ptr, size_t i, unicode c);
int compare_memory21(const void *x, const void *y, size_t unicode_size);
int equal_memory21(const void *x, const void *y, size_t unicode_size);


/*
 *  string21
 */
struct string21_struct {
	size_t size;
	void *ptr;
};
typedef struct string21_struct string21;

string21 *make_string21(size_t size);
string21 *calloc_string21(size_t size);
void free_string21(string21 *str);
void clear_string21(string21 *str);
int getc_string21(const string21 *str, size_t i, unicode *ret);
int putc_string21(string21 *str, size_t i, unicode c);
int compare_string21(const string21 *x, const string21 *y);
int equal_string21(const string21 *x, const string21 *y);

#endif

