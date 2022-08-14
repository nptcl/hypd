#include "base64.h"
#include <stdio.h>
#include <string.h>

#define BASE64_SIZE		0x010000

/*
 *  encode
 */
static int string_equal(const char *x, const char *y, size_t size)
{
	size_t a, b;

	a = strlen(x);
	b = strlen(y);
	if (a != b)
		return 0;
	if (a != size)
		return 0;

	return strcmp(x, y) == 0;
}

int check_encode_call(struct base64_encode *encode, const char *in, const char *out)
{
	char x, y, data[BASE64_SIZE + 1];
	uint8_t c;
	size_t i, k;

	/* encode */
	i = k = 0;
	for (;;) {
		c = (uint8_t)in[i++];
		if (c == 0)
			break;
		base64_encode_pipe(encode, c, &x, &y);
		if (x) {
			data[k++] = x;
			if (BASE64_SIZE <= k)
				return 1;
		}
		if (y) {
			data[k++] = y;
			if (BASE64_SIZE <= k)
				return 1;
		}
	}

	/* closing */
	for (;;) {
		base64_encode_closing(encode, &x);
		if (x == 0)
			break;
		if (x) {
			data[k++] = x;
			if (BASE64_SIZE <= k)
				return 1;
		}
	}

	/* check */
	data[k] = 0;
	if (! string_equal(out, data, k)) {
		fprintf(stderr, "check-encode error, %s, %s, %zu.\n", out, data, k);
		return 1;
	}

	return 0;
}

int check_encode(const char *in, const char *out)
{
	struct base64_encode encode;

	base64_encode_init(&encode);
	return check_encode_call(&encode, in, out);
}

int check_encode_nopadding(const char *in, const char *out)
{
	struct base64_encode encode;

	base64_encode_init(&encode);
	encode.padding = 0;
	return check_encode_call(&encode, in, out);
}


/*
 *  decode
 */
int check_decode_bool(struct base64_decode *decode,
		const char *out, const char *in, int print)
{
	char c, data[BASE64_SIZE + 1];
	uint8_t x;
	int check;
	size_t i, k;

	/* output */
	i = k = 0;
	for (;;) {
		c = in[i++];
		if (c == 0)
			break;
		check = base64_decode_pipe(decode, c, &x);
		if (check < 0)
			return 1;
		if (check == 0)
			continue;
		data[k++] = (char)x;
		if (BASE64_SIZE <= k)
			return 1;
	}

	/* close */
	if (base64_decode_close(decode))
		return 1;

	/* check */
	data[k] = 0;
	if (! string_equal(out, data, k)) {
		if (print)
			fprintf(stderr, "check-decode error, %s, %s, %zu.\n", out, data, k);
		return 1;
	}

	return 0;
}

int check_decode_call(struct base64_decode *decode, const char *out, const char *in)
{
	return check_decode_bool(decode, out, in, 1);
}

int check_decode(const char *out, const char *in)
{
	struct base64_decode decode;

	base64_decode_init(&decode);
	return check_decode_call(&decode, out, in);
}

int check_decode_nopadding(const char *in, const char *out)
{
	struct base64_decode decode;

	base64_decode_init(&decode);
	decode.ignore_padding = 1;
	return check_decode_call(&decode, in, out);
}


/*
 *  test
 */
#define Check_encode(x, y) { \
	printf("Encode: %s, %s\n", x, y); \
	if (check_encode(x, y)) { \
		return 1; \
	} \
}

#define Check_decode(x, y) { \
	printf("Decode: %s, %s\n", x, y); \
	if (check_decode(x, y)) { \
		return 1; \
	} \
}

#define Check_encode_nopadding(x, y) { \
	printf("Encode: %s, %s\n", x, y); \
	if (check_encode_nopadding(x, y)) { \
		return 1; \
	} \
}

#define Check_decode_nopadding(x, y) { \
	printf("Decode: %s, %s\n", x, y); \
	if (check_decode_nopadding(x, y)) { \
		return 1; \
	} \
}


/*
 *  RFC
 */
int test_rfctest(void)
{
	Check_encode("Hello", "SGVsbG8=");
	Check_decode("Hello", "SGVsbG8=");
	Check_encode("", "");
	Check_decode("", "");
	Check_encode("f", "Zg==");
	Check_decode("f", "Zg==");
	Check_encode("fo", "Zm8=");
	Check_decode("fo", "Zm8=");
	Check_encode("foo", "Zm9v");
	Check_decode("foo", "Zm9v");
	Check_encode("foob", "Zm9vYg==");
	Check_decode("foob", "Zm9vYg==");
	Check_encode("fooba", "Zm9vYmE=");
	Check_decode("fooba", "Zm9vYmE=");
	Check_encode("foobar", "Zm9vYmFy");
	Check_decode("foobar", "Zm9vYmFy");

	return 0;
}


/*
 *  tests
 */
int test_tests(void)
{
	Check_encode("", "");
	Check_decode("", "");
	Check_encode("A", "QQ==");
	Check_decode("A", "QQ==");
	Check_encode("AB", "QUI=");
	Check_decode("AB", "QUI=");
	Check_encode("ABC", "QUJD");
	Check_decode("ABC", "QUJD");
	Check_encode("ABCD", "QUJDRA==");
	Check_decode("ABCD", "QUJDRA==");
	Check_encode("ABCDE", "QUJDREU=");
	Check_decode("ABCDE", "QUJDREU=");
	Check_encode("AbCdEfG", "QWJDZEVmRw==");
	Check_decode("AbCdEfG", "QWJDZEVmRw==");
	Check_encode("\xF8", "+A==");
	Check_decode("\xF8", "+A==");
	Check_encode("\xFC", "/A==");
	Check_decode("\xFC", "/A==");

	Check_encode_nopadding("", "");
	Check_decode_nopadding("", "");
	Check_encode_nopadding("A", "QQ");
	Check_decode_nopadding("A", "QQ");
	Check_encode_nopadding("AB", "QUI");
	Check_decode_nopadding("AB", "QUI");
	Check_encode_nopadding("ABC", "QUJD");
	Check_decode_nopadding("ABC", "QUJD");
	Check_encode_nopadding("ABCD", "QUJDRA");
	Check_decode_nopadding("ABCD", "QUJDRA");
	Check_encode_nopadding("ABCDE", "QUJDREU");
	Check_decode_nopadding("ABCDE", "QUJDREU");
	Check_encode_nopadding("AbCdEfG", "QWJDZEVmRw");
	Check_decode_nopadding("AbCdEfG", "QWJDZEVmRw");
	Check_encode_nopadding("\xF8", "+A");
	Check_decode_nopadding("\xF8", "+A");
	Check_encode_nopadding("\xFC", "/A");
	Check_decode_nopadding("\xFC", "/A");

	return 0;
}


/*
 *  encode
 */
static int test_encode_1(void)
{
	char x, y;
	int check;
	struct base64_encode encode;

	printf("Encode: 1\n");
	base64_encode_init(&encode);
	x = 0x7F;
	check = base64_encode_closing(&encode, &x);
	if (check != 0)
		return 1;
	if (encode.state != -1)
		return 1;

	/* closing */
	x = y = 0x7F;
	check = base64_encode_pipe(&encode, 0, &x, &y);
	if (check != -1)
		return 1;
	if (x != 0)
		return 1;
	if (y != 0)
		return 1;

	x = 0x7F;
	check = base64_encode_closing(&encode, &x);
	if (check != -1)
		return 1;
	if (x != 0)
		return 1;

	return 0;
}

static int test_encode_2(void)
{
	struct base64_encode encode;

	printf("Encode: 2\n");
	base64_encode_init(&encode);
	encode.char_padding = '*';
	return check_encode_call(&encode, "A", "QQ**");
}

static int test_encode_3(void)
{
	char x, y;
	struct base64_encode encode;

	printf("Encode: 3\n");

	/* error */
	base64_encode_init(&encode);
	encode.char_padding = '*';
	base64_encode_pipe(&encode, 10, &x, &y);
	if (encode.state != 1)
		return 1;
	base64_encode_clear(&encode);
	return check_encode_call(&encode, "A", "QQ**");
}

static int test_encode_4(void)
{
	struct base64_encode encode;

	printf("Encode: 4\n");

	/* 62 */
	base64_encode_init(&encode);
	encode.char_62 = '*';
	if (check_encode_call(&encode, "\xF8", "*A=="))
		return 1;
	base64_encode_clear(&encode);
	if (check_encode_call(&encode, "\xFC", "/A=="))
		return 1;

	/* 63 */
	base64_encode_init(&encode);
	encode.char_63 = '*';
	if (check_encode_call(&encode, "\xF8", "+A=="))
		return 1;
	base64_encode_clear(&encode);
	if (check_encode_call(&encode, "\xFC", "*A=="))
		return 1;

	return 0;
}

int test_encode(void)
{
	if (test_encode_1())
		return 1;
	if (test_encode_2())
		return 1;
	if (test_encode_3())
		return 1;
	if (test_encode_4())
		return 1;

	return 0;
}

static int test_decode_1(void)
{
	uint8_t x;
	int check;
	struct base64_decode decode;

	printf("Decode: 1\n");
	/* default */
	base64_decode_init(&decode);
	if (decode.ignore_padding != 0)
		return 1;
	if (base64_decode_close(&decode))
		return 1;

	if (decode.state != -1)
		return 1;
	if (! base64_decode_close(&decode))
		return 1;
	if (decode.state != -1)
		return 1;
	check = base64_decode_pipe(&decode, 'F', &x);
	if (check != -1)
		return 1;
	if (decode.state != -1)
		return 1;

	/* ignore_padding */
	base64_decode_init(&decode);
	decode.ignore_padding = 1;
	if (base64_decode_close(&decode))
		return 1;
	if (decode.state != -1)
		return 1;
	if (! base64_decode_close(&decode))
		return 1;
	if (decode.state != -1)
		return 1;
	check = base64_decode_pipe(&decode, 'F', &x);
	if (check != -1)
		return 1;
	if (decode.state != -1)
		return 1;

	return 0;
}

static int test_decode_2(void)
{
	struct base64_decode decode;

	printf("Decode: 2\n");
	base64_decode_init(&decode);
	decode.char_padding = '*';
	return check_decode_call(&decode, "A", "QQ**");
}

static int test_decode_3(void)
{
	uint8_t x;
	struct base64_decode decode;

	printf("Decode: 3\n");
	base64_decode_init(&decode);
	decode.char_padding = '*';
	if (base64_decode_pipe(&decode, 'F', &x) != 0)
		return 1;
	base64_decode_clear(&decode);
	return check_decode_call(&decode, "A", "QQ**");
}

static int test_decode_4(void)
{
	struct base64_decode decode;

	printf("Decode: 4\n");
	/* 62 */
	base64_decode_init(&decode);
	decode.char_62 = '*';
	if (check_decode_call(&decode, "\xF8", "*A=="))
		return 1;
	base64_decode_clear(&decode);
	if (check_decode_call(&decode, "\xFC", "/A=="))
		return 1;

	/* 63 */
	base64_decode_init(&decode);
	decode.char_63 = '*';
	if (check_decode_call(&decode, "\xF8", "+A=="))
		return 1;
	base64_decode_clear(&decode);
	if (check_decode_call(&decode, "\xFC", "*A=="))
		return 1;

	return 0;
}

static int test_decode_ignore_eol(void)
{
	struct base64_decode decode;

	printf("Decode: ignore_eol\n");
	base64_decode_init(&decode);
	if (decode.ignore_eol != 1)
		return 1;
	if (check_decode_call(&decode, "AbCdEfG", "QWJDZEVmRw=="))
		return 1;

	/* 0x0A */
	base64_decode_clear(&decode);
	if (check_decode_call(&decode, "AbCdEfG", "QW\x0AJDZEVmRw=="))
		return 1;

	/* 0x0D */
	base64_decode_clear(&decode);
	if (check_decode_call(&decode, "AbCdEfG", "QW\x0DJDZEVmRw=="))
		return 1;

	/* 0x0D 0x0A */
	base64_decode_clear(&decode);
	if (check_decode_call(&decode, "AbCdEfG", "QW\x0A\x0DJDZEVmRw=="))
		return 1;

	base64_decode_init(&decode);
	decode.ignore_eol = 0;
	if (check_decode_call(&decode, "AbCdEfG", "QWJDZEVmRw=="))
		return 1;

	/* 0x0A */
	base64_decode_clear(&decode);
	if (! check_decode_bool(&decode, "AbCdEfG", "QW\x0AJDZEVmRw==", 0))
		return 1;

	/* 0x0D */
	base64_decode_clear(&decode);
	if (! check_decode_bool(&decode, "AbCdEfG", "QW\x0DJDZEVmRw==", 0))
		return 1;

	/* 0x0D 0x0A */
	base64_decode_clear(&decode);
	if (! check_decode_bool(&decode, "AbCdEfG", "QW\x0A\x0DJDZEVmRw==", 0))
		return 1;

	return 0;
}

static int test_decode_ignore_others(void)
{
	struct base64_decode decode;

	printf("Decode: ignore_others\n");
	base64_decode_init(&decode);
	if (decode.ignore_others != 0)
		return 1;
	if (check_decode_call(&decode, "AbCdEfG", "QWJDZEVmRw=="))
		return 1;

	/* ! */
	base64_decode_clear(&decode);
	if (! check_decode_bool(&decode, "AbCdEfG", "QW!JDZEVmRw==", 0))
		return 1;

	base64_decode_init(&decode);
	decode.ignore_others = 1;
	if (check_decode_call(&decode, "AbCdEfG", "QW!JDZEVmRw=="))
		return 1;

	base64_decode_init(&decode);
	decode.ignore_eol = 1;
	decode.ignore_others = 1;
	if (check_decode_call(&decode, "AbCdEfG", "QW\x0AJDZEVmRw=="))
		return 1;

	base64_decode_init(&decode);
	decode.ignore_eol = 0;
	decode.ignore_others = 1;
	if (! check_decode_bool(&decode, "AbCdEfG", "QW\x0AJDZEVmRw==", 0))
		return 1;

	return 0;
}

static int test_decode_ignore_padding(void)
{
	struct base64_decode decode;

	printf("Decode: ignore_padding\n");
	base64_decode_init(&decode);
	if (decode.ignore_padding != 0)
		return 1;
	if (check_decode_call(&decode, "AbCdEfG", "QWJDZEVmRw=="))
		return 1;

	base64_decode_clear(&decode);
	if (! check_decode_bool(&decode, "AbCdEfG", "QWJDZEVmRw=", 0))
		return 1;

	base64_decode_clear(&decode);
	if (! check_decode_bool(&decode, "AbCdEfG", "QWJDZEVmRw===", 0))
		return 1;

	base64_decode_init(&decode);
	decode.ignore_padding = 1;
	if (check_decode_call(&decode, "AbCdEfG", "QWJDZEVmRw=="))
		return 1;

	base64_decode_clear(&decode);
	if (check_decode_call(&decode, "AbCdEfG", "QWJDZEVmRw="))
		return 1;

	base64_decode_clear(&decode);
	if (check_decode_call(&decode, "AbCdEfG", "QWJDZEVmRw==="))
		return 1;

	return 0;
}

int test_decode(void)
{
	if (test_decode_1())
		return 1;
	if (test_decode_2())
		return 1;
	if (test_decode_3())
		return 1;
	if (test_decode_4())
		return 1;
	if (test_decode_ignore_eol())
		return 1;
	if (test_decode_ignore_others())
		return 1;
	if (test_decode_ignore_padding())
		return 1;

	return 0;
}

int main()
{
	if (test_rfctest()) {
		fprintf(stderr, "test_rfctest error\n");
		return 1;
	}
	if (test_tests()) {
		fprintf(stderr, "test_tests error\n");
		return 1;
	}
	if (test_encode()) {
		fprintf(stderr, "test_encode error\n");
		return 1;
	}
	if (test_decode()) {
		fprintf(stderr, "test_decode error\n");
		return 1;
	}

	return 0;
}

