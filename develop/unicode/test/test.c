#include <stdio.h>
#include <string.h>
#include "unicode.h"

static int Test_ErrorCount = 0;

#define TestGroup(x) { \
	if (x) { \
		printf("Group Error.\n"); \
		return 1; \
	} \
}

#define TestTitle(x) { \
	printf("TestGroup: %s\n", (x)); \
}

#define test(x, y) { \
	printf("Test: %s\n", (y)); \
	if ((x) == 0) { \
		printf("Test Error: %s\n", (y)); \
		Test_ErrorCount++; \
		return 1; \
	} \
}

/*****************************************************************************
 *  UTF-8
 *****************************************************************************/
static int test_init_utf8(void)
{
	struct state_utf8 x;

	TestTitle("*** init_utf8");
	memset(&x, 0xBB, sizeof(x));
	init_utf8(&x);
	test(x.state == 0, "init_utf8.1");

	return 0;
}

static int test_utf8_sequence0(void)
{
	int check;
	unicode c;
	struct state_utf8 x;

	/* switch: -1 */
	init_utf8(&x);
	x.state = -1;
	check = decode_utf8(&x, 0x00, &c);
	test(check == -1, "utf8_sequence0.1");
	test(x.state == -1, "utf8_sequence0.2");
	/* switch: error */
	init_utf8(&x);
	x.state = -2;
	check = decode_utf8(&x, 0x00, &c);
	test(check == -1, "utf8_sequence0.3");
	test(x.state == -1, "utf8_sequence0.4");
	test(x.error == 1, "utf8_sequence0.5");

	/* sequence0: 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0x80, &c);
	test(check == -1, "utf8_sequence0.6");
	test(x.state == -1, "utf8_sequence0.7");
	/* sequence0: 0xC1 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xC1, &c);
	test(check == -1, "utf8_sequence0.8");
	test(x.state == -1, "utf8_sequence0.9");
	/* sequence0: 0xF5 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF5, &c);
	test(check == -1, "utf8_sequence0.10");
	test(x.state == -1, "utf8_sequence0.11");
	/* sequence0: 0xFF */
	init_utf8(&x);
	check = decode_utf8(&x, 0xFF, &c);
	test(check == -1, "utf8_sequence0.12");
	test(x.state == -1, "utf8_sequence0.13");

	return 0;
}

static int test_utf8_sequence1(void)
{
	int check;
	unicode c;
	struct state_utf8 x;

	/* 0x00 */
	init_utf8(&x);
	check = decode_utf8(&x, 0, &c);
	test(check == 1, "utf8_sequence1.1");
	test(c == 0, "utf8_sequence1.2");
	test(x.state == 0, "utf8_sequence1.3");

	/* 0x55 */
	check = decode_utf8(&x, 0x55, &c);
	test(check == 1, "utf8_sequence1.4");
	test(c == 0x55, "utf8_sequence1.5");
	test(x.state == 0, "utf8_sequence1.6");

	/* 0x7F */
	check = decode_utf8(&x, 0x7F, &c);
	test(check == 1, "utf8_sequence1.7");
	test(c == 0x7F, "utf8_sequence1.8");
	test(x.state == 0, "utf8_sequence1.9");

	/* 0x00 0x7F */
	check = decode_utf8(&x, 0x00, &c);
	test(check == 1, "utf8_sequence1.10");
	check = decode_utf8(&x, 0x7F, &c);
	test(check == 1, "utf8_sequence1.11");
	test(c == 0x7F, "utf8_sequence1.12");
	test(x.state == 0, "utf8_sequence1.13");

	return 0;
}

static int test_utf8_sequence2(void)
{
	int check;
	unicode c;
	struct state_utf8 x;

	/* 0xC2 0x7F */
	init_utf8(&x);
	check = decode_utf8(&x, 0xC2, &c);
	test(check == 0, "utf8_sequence2.1");
	check = decode_utf8(&x, 0x7F, &c);
	test(check == -1, "utf8_sequence2.2");
	/* 0xC2 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xC2, &c);
	check = decode_utf8(&x, 0x80, &c);
	test(check == 1, "utf8_sequence2.3");
	test(c == 0x80, "utf8_sequence2.4");
	/* 0xDF 0xBF */
	init_utf8(&x);
	check = decode_utf8(&x, 0xDF, &c);
	check = decode_utf8(&x, 0xBF, &c);
	test(check == 1, "utf8_sequence2.5");
	test(c == 0x07FF, "utf8_sequence2.6");
	/* 0xDF 0xC0 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xDF, &c);
	check = decode_utf8(&x, 0xC0, &c);
	test(check == -1, "utf8_sequence2.7");

	/* 0xC2 0x80, 0x20 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xC2, &c);
	check = decode_utf8(&x, 0x80, &c);
	test(check == 1, "utf8_sequence2.8");
	test(c == 0x80, "utf8_sequence2.9");
	test(x.state == 0, "utf8_sequence2.10");
	check = decode_utf8(&x, 0x20, &c);
	test(check == 1, "utf8_sequence2.11");
	test(c == 0x20, "utf8_sequence2.12");
	test(x.state == 0, "utf8_sequence2.13");

	return 0;
}

static int test_utf8_sequence3(void)
{
	int check;
	unicode c;
	struct state_utf8 x;

	/* 0xE0 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xE0, &c);
	test(check == 0, "utf8_sequence3.1");

	/* 0xEA 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xEA, &c);
	test(check == 0, "utf8_sequence3.2");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 0, "utf8_sequence3.3");

	/* Error: 0xE0 0x80 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xE0, &c);
	test(check == 0, "utf8_sequence3.low.1");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 0, "utf8_sequence3.low.2");
	check = decode_utf8(&x, 0x80, &c);
	test(check == -1, "utf8_sequence3.low.3");

	/* Error: 0xE0 0x9F 0xBF */
	init_utf8(&x);
	check = decode_utf8(&x, 0xE0, &c);
	test(check == 0, "utf8_sequence3.low.4");
	check = decode_utf8(&x, 0x9F, &c);
	test(check == 0, "utf8_sequence3.low.5");
	check = decode_utf8(&x, 0xBF, &c);
	test(check == -1, "utf8_sequence3.low.6");

	/* 0xE0 0xA0 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xE0, &c);
	test(check == 0, "utf8_sequence3.low.7");
	check = decode_utf8(&x, 0xA0, &c);
	test(check == 0, "utf8_sequence3.low.8");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 1, "utf8_sequence3.low.9");
	test(c == 0x0800, "utf8_sequence3.low.10");

	/* 0xE0 0xA0 0x81 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xE0, &c);
	test(check == 0, "utf8_sequence3.low.11");
	check = decode_utf8(&x, 0xA0, &c);
	test(check == 0, "utf8_sequence3.low.12");
	check = decode_utf8(&x, 0x81, &c);
	test(check == 1, "utf8_sequence3.low.13");
	test(c == 0x0801, "utf8_sequence3.low.14");

	/* 0xED 0x9F 0xBF */
	init_utf8(&x);
	check = decode_utf8(&x, 0xED, &c);
	test(check == 0, "utf8_sequence3.surrogate.1");
	check = decode_utf8(&x, 0x9F, &c);
	test(check == 0, "utf8_sequence3.surrogate.2");
	check = decode_utf8(&x, 0xBF, &c);
	test(check == 1, "utf8_sequence3.surrogate.3");
	test(c == 0xD7FF, "utf8_sequence3.surrogate.4");

	/* Error: 0xED 0xA0 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xED, &c);
	test(check == 0, "utf8_sequence3.surrogate.5");
	check = decode_utf8(&x, 0xA0, &c);
	test(check == 0, "utf8_sequence3.surrogate.6");
	check = decode_utf8(&x, 0x80, &c);
	test(check == -1, "utf8_sequence3.surrogate.7");
	test(x.error == 4, "utf8_sequence3.surrogate.8");

	/* Error: 0xED 0xBE 0xBE */
	init_utf8(&x);
	check = decode_utf8(&x, 0xED, &c);
	test(check == 0, "utf8_sequence3.surrogate.9");
	check = decode_utf8(&x, 0xBE, &c);
	test(check == 0, "utf8_sequence3.surrogate.10");
	check = decode_utf8(&x, 0xBE, &c);
	test(check == -1, "utf8_sequence3.surrogate.11");
	test(x.error == 4, "utf8_sequence3.surrogate.12");

	/* 0xEE 0x80 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xEE, &c);
	test(check == 0, "utf8_sequence3.surrogate.13");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 0, "utf8_sequence3.surrogate.14");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 1, "utf8_sequence3.surrogate.15");
	test(c == 0xE000, "utf8_sequence3.surrogate.16");

	/* 0xEF 0xBF 0xBF */
	init_utf8(&x);
	check = decode_utf8(&x, 0xEF, &c);
	test(check == 0, "utf8_sequence3.high.1");
	check = decode_utf8(&x, 0xBF, &c);
	test(check == 0, "utf8_sequence3.high.2");
	check = decode_utf8(&x, 0xBF, &c);
	test(check == 1, "utf8_sequence3.high.3");
	test(c == 0xFFFF, "utf8_sequence3.high.4");

	/* Error: 0xE0 0x7F */
	init_utf8(&x);
	check = decode_utf8(&x, 0xE0, &c);
	test(check == 0, "utf8_sequence3.range.1");
	check = decode_utf8(&x, 0x7F, &c);
	test(check == -1, "utf8_sequence3.range.2");

	/* Error: 0xE0 0xC0 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xE0, &c);
	test(check == 0, "utf8_sequence3.range.3");
	check = decode_utf8(&x, 0xC0, &c);
	test(check == -1, "utf8_sequence3.range.4");

	/* Error: 0xEA 0x80 0x7F */
	init_utf8(&x);
	check = decode_utf8(&x, 0xEA, &c);
	test(check == 0, "utf8_sequence3.range.5");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 0, "utf8_sequence3.range.6");
	check = decode_utf8(&x, 0x7F, &c);
	test(check == -1, "utf8_sequence3.range.7");

	/* Error: 0xEA 0x80 0xC0 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xEA, &c);
	test(check == 0, "utf8_sequence3.range.8");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 0, "utf8_sequence3.range.9");
	check = decode_utf8(&x, 0xC0, &c);
	test(check == -1, "utf8_sequence3.range.10");

	/* 0xE0 0xA0 0x80, 0x20 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xE0, &c);
	test(check == 0, "utf8_sequence3.next.1");
	check = decode_utf8(&x, 0xA0, &c);
	test(check == 0, "utf8_sequence3.next.2");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 1, "utf8_sequence3.next.3");
	test(c == 0x0800, "utf8_sequence3.next.4");
	test(x.state == 0, "utf8_sequence3.next.5");
	check = decode_utf8(&x, 0x20, &c);
	test(check == 1, "utf8_sequence3.next.6");
	test(c == 0x20, "utf8_sequence3.next.7");

	return 0;
}

static int test_utf8_sequence4(void)
{
	int check;
	unicode c;
	struct state_utf8 x;

	/* 0xF0 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF0, &c);
	test(check == 0, "utf8_sequence4.1");

	/* Error: 0xF0 0x80 0x80 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF0, &c);
	test(check == 0, "utf8_sequence4.low.1");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 0, "utf8_sequence4.low.2");
	check = decode_utf8(&x, 0x80, &c);
	test(check == 0, "utf8_sequence4.low.3");
	check = decode_utf8(&x, 0x80, &c);
	test(check == -1, "utf8_sequence4.low.4");

	/* Error: 0xF0 0x8F 0xBF 0xBF */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF0, &c);
	test(check == 0, "utf8_sequence4.low.5");
	check = decode_utf8(&x, 0x8F, &c);
	test(check == 0, "utf8_sequence4.low.6");
	check = decode_utf8(&x, 0xBF, &c);
	test(check == 0, "utf8_sequence4.low.7");
	check = decode_utf8(&x, 0xBF, &c);
	test(check == -1, "utf8_sequence4.low.8");

	/* 0xF0 0x90 0x80 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF0, &c);
	check = decode_utf8(&x, 0x90, &c);
	check = decode_utf8(&x, 0x80, &c);
	check = decode_utf8(&x, 0x80, &c);
	test(check == 1, "utf8_sequence4.low.9");
	test(c == 0x010000, "utf8_sequence4.low.10");

	/* 0xF0 0x90 0x80 0x81 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF0, &c);
	check = decode_utf8(&x, 0x90, &c);
	check = decode_utf8(&x, 0x80, &c);
	check = decode_utf8(&x, 0x81, &c);
	test(check == 1, "utf8_sequence4.low.11");
	test(c == 0x010001, "utf8_sequence4.low.12");

	/* 0xF4 0x8F 0xBF 0xBF */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF4, &c);
	check = decode_utf8(&x, 0x8F, &c);
	check = decode_utf8(&x, 0xBF, &c);
	check = decode_utf8(&x, 0xBF, &c);
	test(check == 1, "utf8_sequence4.low.13");
	test(c == 0x10FFFF, "utf8_sequence4.low.14");

	/* Error: 0xF4 0x90 0x80 0x80 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF4, &c);
	check = decode_utf8(&x, 0x90, &c);
	check = decode_utf8(&x, 0x80, &c);
	test(check == 0, "utf8_sequence4.low.15");
	check = decode_utf8(&x, 0x80, &c);
	test(check == -1, "utf8_sequence4.low.16");

	/* Error: 0xF1 0x7F */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF1, &c);
	check = decode_utf8(&x, 0x7F, &c);
	test(check == -1, "utf8_sequence4.low.17");

	/* Error: 0xF1 0xC0 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF1, &c);
	check = decode_utf8(&x, 0xC0, &c);
	test(check == -1, "utf8_sequence4.low.18");

	/* Error: 0xF1 0x80 0x7F */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF1, &c);
	check = decode_utf8(&x, 0x80, &c);
	check = decode_utf8(&x, 0x7F, &c);
	test(check == -1, "utf8_sequence4.low.19");

	/* Error: 0xF1 0x80 0xC0 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF1, &c);
	check = decode_utf8(&x, 0x80, &c);
	check = decode_utf8(&x, 0xC0, &c);
	test(check == -1, "utf8_sequence4.low.20");

	/* Error: 0xF1 0x80 0x80 0x7F */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF1, &c);
	check = decode_utf8(&x, 0x80, &c);
	check = decode_utf8(&x, 0x80, &c);
	check = decode_utf8(&x, 0x7F, &c);
	test(check == -1, "utf8_sequence4.low.21");

	/* Error: 0xF1 0x80 0x80 0xC0 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF1, &c);
	check = decode_utf8(&x, 0x80, &c);
	check = decode_utf8(&x, 0x80, &c);
	check = decode_utf8(&x, 0xC0, &c);
	test(check == -1, "utf8_sequence4.low.22");

	/* 0xF0 0x90 0x80 0x80, 0x30 */
	init_utf8(&x);
	check = decode_utf8(&x, 0xF0, &c);
	check = decode_utf8(&x, 0x90, &c);
	check = decode_utf8(&x, 0x80, &c);
	check = decode_utf8(&x, 0x80, &c);
	test(check == 1, "utf8_sequence4.next.1");
	test(c == 0x010000, "utf8_sequence4.next.2");
	test(x.state == 0, "utf8_sequence4.next.3");
	check = decode_utf8(&x, 0x30, &c);
	test(check == 1, "utf8_sequence4.next.4");
	test(c == 0x30, "utf8_sequence4.next.5");

	return 0;
}

static int test_decode_utf8(void)
{
	TestTitle("*** decode_utf8");
	TestGroup(test_utf8_sequence0());
	TestGroup(test_utf8_sequence1());
	TestGroup(test_utf8_sequence2());
	TestGroup(test_utf8_sequence3());
	TestGroup(test_utf8_sequence4());

	return 0;
}

static int test_encode_utf8(void)
{
	uint8_t x[10];
	int y;

	TestTitle("*** encode_utf8");
	/* 0x00 */
	test(encode_utf8(0, x, &y) == 0, "encode_utf8.1");
	test(y == 1, "encode_utf8.2");
	test(x[0] == 0, "encode_utf8.3");
	/* 0x20 */
	test(encode_utf8(0x20, x, &y) == 0, "encode_utf8.4");
	test(y == 1, "encode_utf8.5");
	test(x[0] == 0x20, "encode_utf8.6");
	/* Error: -1 */
	test(encode_utf8(-1, x, &y) == 1, "encode_utf8.7");
	test(y == 0, "encode_utf8.8");

	/* 0x7F */
	test(encode_utf8(0x7F, x, &y) == 0, "encode_utf8.2byte.1");
	test(y == 1, "encode_utf8.2byte.2");
	test(x[0] == 0x7F, "encode_utf8.2byte.3");
	/* 0x80 -> 0xC2 0x80 */
	test(encode_utf8(0x80, x, &y) == 0, "encode_utf8.2byte.4");
	test(y == 2, "encode_utf8.2byte.5");
	test(x[0] == 0xC2, "encode_utf8.2byte.6");
	test(x[1] == 0x80, "encode_utf8.2byte.7");
	/* 0x81 -> 0xC2 0x81 */
	test(encode_utf8(0x81, x, &y) == 0, "encode_utf8.2byte.8");
	test(y == 2, "encode_utf8.2byte.9");
	test(x[0] == 0xC2, "encode_utf8.2byte.10");
	test(x[1] == 0x81, "encode_utf8.2byte.11");

	/* 0x07FF -> 0xDF 0xBF */
	test(encode_utf8(0x07FF, x, &y) == 0, "encode_utf8.2byte.12");
	test(y == 2, "encode_utf8.2byte.13");
	test(x[0] == 0xDF, "encode_utf8.2byte.14");
	test(x[1] == 0xBF, "encode_utf8.2byte.15");

	/* 0x0800 -> 0xE0 0xA0 0x80 */
	test(encode_utf8(0x0800, x, &y) == 0, "encode_utf8.3byte.1");
	test(y == 3, "encode_utf8.3byte.2");
	test(x[0] == 0xE0, "encode_utf8.3byte.3");
	test(x[1] == 0xA0, "encode_utf8.3byte.4");
	test(x[2] == 0x80, "encode_utf8.3byte.5");

	/* 0x0801 -> 0xE0 0xA0 0x81 */
	test(encode_utf8(0x0801, x, &y) == 0, "encode_utf8.3byte.6");
	test(y == 3, "encode_utf8.3byte.7");
	test(x[0] == 0xE0, "encode_utf8.3byte.8");
	test(x[1] == 0xA0, "encode_utf8.3byte.9");
	test(x[2] == 0x81, "encode_utf8.3byte.10");

	/* 0xD7FF -> 0xED 0x9F 0xBF */
	test(encode_utf8(0xD7FF, x, &y) == 0, "encode_utf8.3byte.11");
	test(y == 3, "encode_utf8.3byte.12");
	test(x[0] == 0xED, "encode_utf8.3byte.13");
	test(x[1] == 0x9F, "encode_utf8.3byte.14");
	test(x[2] == 0xBF, "encode_utf8.3byte.15");

	/* Error: 0xD800 */
	test(encode_utf8(0xD800, x, &y) == 1, "encode_utf8.3byte.16");
	test(y == 0, "encode_utf8.3byte.17");

	/* Error: 0xDFFF */
	test(encode_utf8(0xDFFF, x, &y) == 1, "encode_utf8.3byte.18");
	test(y == 0, "encode_utf8.3byte.19");

	/* 0xE000 -> 0xEE 0x80 0x80 */
	test(encode_utf8(0xE000, x, &y) == 0, "encode_utf8.3byte.20");
	test(y == 3, "encode_utf8.3byte.21");
	test(x[0] == 0xEE, "encode_utf8.3byte.22");
	test(x[1] == 0x80, "encode_utf8.3byte.23");
	test(x[2] == 0x80, "encode_utf8.3byte.24");

	/* 0xE001 -> 0xEE 0x80 0x81 */
	test(encode_utf8(0xE001, x, &y) == 0, "encode_utf8.3byte.25");
	test(y == 3, "encode_utf8.3byte.26");
	test(x[0] == 0xEE, "encode_utf8.3byte.27");
	test(x[1] == 0x80, "encode_utf8.3byte.28");
	test(x[2] == 0x81, "encode_utf8.3byte.29");

	/* 0xFFFF -> 0xEF 0xBF 0xBF */
	test(encode_utf8(0xFFFF, x, &y) == 0, "encode_utf8.3byte.30");
	test(y == 3, "encode_utf8.3byte.31");
	test(x[0] == 0xEF, "encode_utf8.3byte.32");
	test(x[1] == 0xBF, "encode_utf8.3byte.33");
	test(x[2] == 0xBF, "encode_utf8.3byte.34");

	/* 0x010000 -> 0xF0 0x90 0x80 0x80 */
	test(encode_utf8(0x010000, x, &y) == 0, "encode_utf8.4byte.1");
	test(y == 4, "encode_utf8.4byte.2");
	test(x[0] == 0xF0, "encode_utf8.4byte.3");
	test(x[1] == 0x90, "encode_utf8.4byte.4");
	test(x[2] == 0x80, "encode_utf8.4byte.5");
	test(x[3] == 0x80, "encode_utf8.4byte.6");

	/* 0x010001 -> 0xF0 0x90 0x80 0x81 */
	test(encode_utf8(0x010001, x, &y) == 0, "encode_utf8.4byte.7");
	test(y == 4, "encode_utf8.4byte.8");
	test(x[0] == 0xF0, "encode_utf8.4byte.9");
	test(x[1] == 0x90, "encode_utf8.4byte.10");
	test(x[2] == 0x80, "encode_utf8.4byte.11");
	test(x[3] == 0x81, "encode_utf8.4byte.12");

	/* 0x10FFFF -> 0xF4 0x8F 0xBF 0xBF */
	test(encode_utf8(0x10FFFF, x, &y) == 0, "encode_utf8.4byte.13");
	test(y == 4, "encode_utf8.4byte.14");
	test(x[0] == 0xF4, "encode_utf8.4byte.15");
	test(x[1] == 0x8F, "encode_utf8.4byte.16");
	test(x[2] == 0xBF, "encode_utf8.4byte.17");
	test(x[3] == 0xBF, "encode_utf8.4byte.18");

	/* Error: 0x110000 */
	test(encode_utf8(0x110000, x, &y) == 1, "encode_utf8.4byte.19");
	test(y == 0, "encode_utf8.4byte.20");

	return 0;
}

static int test_length_utf8(void)
{
	unsigned x;

	TestTitle("*** length_utf8");

	test(length_utf8(0, &x) == 0, "length_utf8.1");
	test(x == 1, "length_utf8.2");

	test(length_utf8(-1, &x) == 1, "length_utf8.3");
	test(x == 0, "length_utf8.4");

	test(length_utf8(0x20, &x) == 0, "length_utf8.5");
	test(x == 1, "length_utf8.6");

	test(length_utf8(0x7F, &x) == 0, "length_utf8.7");
	test(x == 1, "length_utf8.8");

	test(length_utf8(0x80, &x) == 0, "length_utf8.9");
	test(x == 2, "length_utf8.10");

	test(length_utf8(0x07FF, &x) == 0, "length_utf8.11");
	test(x == 2, "length_utf8.12");

	test(length_utf8(0x0800, &x) == 0, "length_utf8.13");
	test(x == 3, "length_utf8.14");

	test(length_utf8(0xD7FF, &x) == 0, "length_utf8.15");
	test(x == 3, "length_utf8.16");

	test(length_utf8(0xD800, &x) == 1, "length_utf8.17");
	test(x == 0, "length_utf8.18");

	test(length_utf8(0xDFFF, &x) == 1, "length_utf8.19");
	test(x == 0, "length_utf8.20");

	test(length_utf8(0xE000, &x) == 0, "length_utf8.21");
	test(x == 3, "length_utf8.22");

	test(length_utf8(0xFFFF, &x) == 0, "length_utf8.23");
	test(x == 3, "length_utf8.24");

	test(length_utf8(0x010000, &x) == 0, "length_utf8.25");
	test(x == 4, "length_utf8.26");

	test(length_utf8(0x10FFFF, &x) == 0, "length_utf8.27");
	test(x == 4, "length_utf8.28");

	test(length_utf8(0x110000, &x) == 1, "length_utf8.29");
	test(x == 0, "length_utf8.30");

	return 0;
}


/*****************************************************************************
 *  UTF-16
 *****************************************************************************/
static int test_init_utf16(void)
{
	struct state_utf16 x;

	TestTitle("*** init_utf16");
	memset(&x, 0xBB, sizeof(x));
	init_utf16(&x);
	test(x.state == 0, "init_utf16.1");

	return 0;
}

static int test_decode_utf16(void)
{
	unicode c;
	struct state_utf16 x;

	TestTitle("*** decode_utf16");
	init_utf16(&x);
	test(decode_utf16(&x, 'A', &c) == 1, "decode_utf16.1");
	test(c == 'A', "decode_utf16.2");
	test(decode_utf16(&x, 'B', &c) == 1, "decode_utf16.3");
	test(c == 'B', "decode_utf16.4");

	test(decode_utf16(&x, 0xD7FF, &c) == 1, "decode_utf16.5");
	test(c == 0xD7FF, "decode_utf16.6");
	test(decode_utf16(&x, 0xDC00, &c) == -1, "decode_utf16.7");

	init_utf16(&x);
	test(decode_utf16(&x, 0xDFFF, &c) == -1, "decode_utf16.8");

	init_utf16(&x);
	test(decode_utf16(&x, 0xE000, &c) == 1, "decode_utf16.9");
	test(c == 0xE000, "decode_utf16.10");
	test(decode_utf16(&x, 0xFFFF, &c) == 1, "decode_utf16.11");
	test(c == 0xFFFF, "decode_utf16.12");

	/* sequence2 */
	init_utf16(&x);
	test(decode_utf16(&x, 0xD800, &c) == 0, "decode_utf16.seq2.1");
	test(decode_utf16(&x, 0x00, &c) == -1, "decode_utf16.seq2.2");

	init_utf16(&x);
	test(decode_utf16(&x, 0xD900, &c) == 0, "decode_utf16.seq2.3");
	test(decode_utf16(&x, 0xDBFF, &c) == -1, "decode_utf16.seq2.4");

	init_utf16(&x);
	test(decode_utf16(&x, 0xD800, &c) == 0, "decode_utf16.seq2.5");
	test(decode_utf16(&x, 0xDC00, &c) == 1, "decode_utf16.seq2.6");
	test(c == 0x010000, "decode_utf16.seq2.7");

	init_utf16(&x);
	test(decode_utf16(&x, 0xD800, &c) == 0, "decode_utf16.seq2.8");
	test(decode_utf16(&x, 0xDFFF, &c) == 1, "decode_utf16.seq2.9");
	test(c == 0x0103FF, "decode_utf16.seq2.10");

	init_utf16(&x);
	test(decode_utf16(&x, 0xD800, &c) == 0, "decode_utf16.seq2.11");
	test(decode_utf16(&x, 0xE000, &c) == -1, "decode_utf16.seq2.12");

	init_utf16(&x);
	test(decode_utf16(&x, 0xD801, &c) == 0, "decode_utf16.seq2.13");
	test(decode_utf16(&x, 0xDC00, &c) == 1, "decode_utf16.seq2.14");
	test(c == 0x010400, "decode_utf16.seq2.14");

	init_utf16(&x);
	test(decode_utf16(&x, 0xDBFF, &c) == 0, "decode_utf16.seq2.15");
	test(decode_utf16(&x, 0xDFFF, &c) == 1, "decode_utf16.seq2.16");
	test(c == 0x10FFFF, "decode_utf16.seq2.17");

	init_utf16(&x);
	test(decode_utf16(&x, 0xDC00, &c) == -1, "decode_utf16.seq2.18");

	return 0;
}

static int test_encode_utf16(void)
{
	uint16_t x[2];
	int y;

	TestTitle("*** encode_utf16");
	test(encode_utf16(0, x, &y) == 0, "encode_utf16.1");
	test(y == 1, "encode_utf16.2");
	test(x[0] == 0, "encode_utf16.3");

	test(encode_utf16('A', x, &y) == 0, "encode_utf16.4");
	test(y == 1, "encode_utf16.5");
	test(x[0] == 'A', "encode_utf16.6");

	test(encode_utf16(-1, x, &y) == 1, "encode_utf16.7");

	test(encode_utf16(0xD7FF, x, &y) == 0, "encode_utf16.8");
	test(y == 1, "encode_utf16.9");
	test(x[0] == 0xD7FF, "encode_utf16.10");

	test(encode_utf16(0xD800, x, &y) == 1, "encode_utf16.11");

	test(encode_utf16(0xDFFF, x, &y) == 1, "encode_utf16.12");

	test(encode_utf16(0xE000, x, &y) == 0, "encode_utf16.13");
	test(y == 1, "encode_utf16.14");
	test(x[0] == 0xE000, "encode_utf16.15");

	test(encode_utf16(0xFFFF, x, &y) == 0, "encode_utf16.16");
	test(y == 1, "encode_utf16.17");
	test(x[0] == 0xFFFF, "encode_utf16.18");

	test(encode_utf16(0x010000, x, &y) == 0, "encode_utf16.seq2.1");
	test(y == 2, "encode_utf16.seq2.2");
	test(x[0] == 0xD800, "encode_utf16.seq2.3");
	test(x[1] == 0xDC00, "encode_utf16.seq2.4");

	test(encode_utf16(0x010FFFF, x, &y) == 0, "encode_utf16.seq2.5");
	test(y == 2, "encode_utf16.seq2.6");
	test(x[0] == 0xDBFF, "encode_utf16.seq2.7");
	test(x[1] == 0xDFFF, "encode_utf16.seq2.8");

	test(encode_utf16(0x0110000, x, &y) == 1, "encode_utf16.seq2.9");

	return 0;
}

static int test_length_utf16(void)
{
	unsigned x;

	TestTitle("*** length_utf16");
	test(length_utf16(-1, &x) == 1, "length_utf16.1");

	test(length_utf16(0, &x) == 0, "length_utf16.2");
	test(x == 1, "length_utf16.3");

	test(length_utf16('A', &x) == 0, "length_utf16.4");
	test(x == 1, "length_utf16.5");

	test(length_utf16(0xD7FF, &x) == 0, "length_utf16.6");
	test(x == 1, "length_utf16.7");

	test(length_utf16(0xD800, &x) == 1, "length_utf16.8");

	test(length_utf16(0xDFFF, &x) == 1, "length_utf16.9");

	test(length_utf16(0xE000, &x) == 0, "length_utf16.10");
	test(x == 1, "length_utf16.11");

	test(length_utf16(0xFFFF, &x) == 0, "length_utf16.12");
	test(x == 1, "length_utf16.13");

	test(length_utf16(0x010000, &x) == 0, "length_utf16.14");
	test(x == 2, "length_utf16.15");

	test(length_utf16(0x10FFFF, &x) == 0, "length_utf16.16");
	test(x == 2, "length_utf16.17");

	test(length_utf16(0x110000, &x) == 1, "length_utf16.18");

	return 0;
}


/*****************************************************************************
 *  UTF-16LE
 *****************************************************************************/
static int test_init_utf16le(void)
{
	struct state_utf16le x;

	TestTitle("*** init_utf16le");
	memset(&x, 0xBB, sizeof(x));
	init_utf16le(&x);
	test(x.state == 0, "init_utf16le.1");

	return 0;
}

static int test_decode_utf16le(void)
{
	unicode c;
	struct state_utf16le x;

	TestTitle("*** decode_utf16le");
	init_utf16le(&x);
	test(decode_utf16le(&x, 0x00, &c) == 0, "decode_utf16le.1");
	test(decode_utf16le(&x, 0x00, &c) == 1, "decode_utf16le.2");
	test(c == 0x0000, "decode_utf16le.3");

	test(decode_utf16le(&x, 'A', &c) == 0, "decode_utf16le.4");
	test(decode_utf16le(&x, 0x00, &c) == 1, "decode_utf16le.5");
	test(c == 'A', "decode_utf16le.6");

	test(decode_utf16le(&x, 0x12, &c) == 0, "decode_utf16le.7");
	test(decode_utf16le(&x, 0x34, &c) == 1, "decode_utf16le.8");
	test(c == 0x3412, "decode_utf16le.9");

	init_utf16le(&x);
	test(decode_utf16le(&x, 0x00, &c) == 0, "decode_utf16le.10");
	test(decode_utf16le(&x, 0xD8, &c) == 0, "decode_utf16le.11");
	test(decode_utf16le(&x, 0x0A, &c) == 0, "decode_utf16le.12");
	test(decode_utf16le(&x, 0xDC, &c) == 1, "decode_utf16le.13");
	test(c == 0x01000A, "decode_utf16le.14");

	init_utf16le(&x);
	test(decode_utf16le(&x, 0x00, &c) == 0, "decode_utf16le.15");
	test(decode_utf16le(&x, 0xDC, &c) == -1, "decode_utf16le.16");

	init_utf16le(&x);
	test(decode_utf16le(&x, 0x00, &c) == 0, "decode_utf16le.17");
	test(decode_utf16le(&x, 0xD8, &c) == 0, "decode_utf16le.18");
	test(decode_utf16le(&x, 0x00, &c) == 0, "decode_utf16le.19");
	test(decode_utf16le(&x, 0xD8, &c) == -1, "decode_utf16le.20");

	return 0;
}

static int test_encode_utf16le(void)
{
	uint8_t x[4];
	int y;

	TestTitle("*** encode_utf16le");
	test(encode_utf16le(0, x, &y) == 0, "encode_utf16le.1");
	test(y == 2, "encode_utf16le.2");
	test(x[0] == 0, "encode_utf16le.3");
	test(x[1] == 0, "encode_utf16le.4");

	test(encode_utf16le('A', x, &y) == 0, "encode_utf16le.5");
	test(y == 2, "encode_utf16le.6");
	test(x[0] == 'A', "encode_utf16le.7");
	test(x[1] == 0, "encode_utf16le.8");

	test(encode_utf16le(-1, x, &y) == 1, "encode_utf16le.9");

	test(encode_utf16le(0xD7FF, x, &y) == 0, "encode_utf16le.10");
	test(y == 2, "encode_utf16le.11");
	test(x[0] == 0xFF, "encode_utf16le.12");
	test(x[1] == 0xD7, "encode_utf16le.13");

	test(encode_utf16le(0xD800, x, &y) == 1, "encode_utf16le.14");

	test(encode_utf16le(0x01000A, x, &y) == 0, "encode_utf16le.seq2.1");
	test(y == 4, "encode_utf16le.seq2.2");
	test(x[0] == 0x00, "encode_utf16le.seq2.3");
	test(x[1] == 0xD8, "encode_utf16le.seq2.4");
	test(x[2] == 0x0A, "encode_utf16le.seq2.5");
	test(x[3] == 0xDC, "encode_utf16le.seq2.6");

	test(encode_utf16le(0x0110000, x, &y) == 1, "encode_utf16le.seq2.7");

	return 0;
}

static int test_length_utf16le(void)
{
	unsigned x;

	TestTitle("*** length_utf16le");
	test(length_utf16le(-1, &x) == 1, "length_utf16le.1");

	test(length_utf16le(0, &x) == 0, "length_utf16le.2");
	test(x == 2, "length_utf16le.3");

	test(length_utf16le('A', &x) == 0, "length_utf16le.4");
	test(x == 2, "length_utf16le.5");

	test(length_utf16le(0xD800, &x) == 1, "length_utf16le.6");

	test(length_utf16le(0x010000, &x) == 0, "length_utf16le.7");
	test(x == 4, "length_utf16le.8");

	test(length_utf16le(0x110000, &x) == 1, "length_utf16le.17");

	return 0;
}


/*****************************************************************************
 *  UTF-16BE
 *****************************************************************************/
static int test_init_utf16be(void)
{
	struct state_utf16be x;

	TestTitle("*** init_utf16be");
	memset(&x, 0xBB, sizeof(x));
	init_utf16be(&x);
	test(x.state == 0, "init_utf16be.1");

	return 0;
}

static int test_decode_utf16be(void)
{
	unicode c;
	struct state_utf16be x;

	TestTitle("*** decode_utf16be");
	init_utf16be(&x);
	test(decode_utf16be(&x, 0x00, &c) == 0, "decode_utf16be.1");
	test(decode_utf16be(&x, 0x00, &c) == 1, "decode_utf16be.2");
	test(c == 0x0000, "decode_utf16be.3");

	test(decode_utf16be(&x, 0x00, &c) == 0, "decode_utf16be.4");
	test(decode_utf16be(&x, 'A', &c) == 1, "decode_utf16be.5");
	test(c == 'A', "decode_utf16be.6");

	test(decode_utf16be(&x, 0x12, &c) == 0, "decode_utf16be.7");
	test(decode_utf16be(&x, 0x34, &c) == 1, "decode_utf16be.8");
	test(c == 0x1234, "decode_utf16be.9");

	init_utf16be(&x);
	test(decode_utf16be(&x, 0xD8, &c) == 0, "decode_utf16be.10");
	test(decode_utf16be(&x, 0x00, &c) == 0, "decode_utf16be.11");
	test(decode_utf16be(&x, 0xDC, &c) == 0, "decode_utf16be.12");
	test(decode_utf16be(&x, 0x0A, &c) == 1, "decode_utf16be.13");
	test(c == 0x01000A, "decode_utf16be.14");

	init_utf16be(&x);
	test(decode_utf16be(&x, 0xDC, &c) == 0, "decode_utf16be.15");
	test(decode_utf16be(&x, 0x00, &c) == -1, "decode_utf16be.16");

	init_utf16be(&x);
	test(decode_utf16be(&x, 0xD8, &c) == 0, "decode_utf16be.17");
	test(decode_utf16be(&x, 0x00, &c) == 0, "decode_utf16be.18");
	test(decode_utf16be(&x, 0xD8, &c) == 0, "decode_utf16be.19");
	test(decode_utf16be(&x, 0x00, &c) == -1, "decode_utf16be.20");

	return 0;
}

static int test_encode_utf16be(void)
{
	uint8_t x[4];
	int y;

	TestTitle("*** encode_utf16be");
	test(encode_utf16be(0, x, &y) == 0, "encode_utf16be.1");
	test(y == 2, "encode_utf16be.2");
	test(x[0] == 0, "encode_utf16be.3");
	test(x[1] == 0, "encode_utf16be.4");

	test(encode_utf16be('A', x, &y) == 0, "encode_utf16be.5");
	test(y == 2, "encode_utf16be.6");
	test(x[0] == 0, "encode_utf16be.7");
	test(x[1] == 'A', "encode_utf16be.8");

	test(encode_utf16be(-1, x, &y) == 1, "encode_utf16be.9");

	test(encode_utf16be(0xD7FF, x, &y) == 0, "encode_utf16be.10");
	test(y == 2, "encode_utf16be.11");
	test(x[0] == 0xD7, "encode_utf16be.12");
	test(x[1] == 0xFF, "encode_utf16be.13");

	test(encode_utf16be(0xD800, x, &y) == 1, "encode_utf16be.14");

	test(encode_utf16be(0x01000A, x, &y) == 0, "encode_utf16be.seq2.1");
	test(y == 4, "encode_utf16be.seq2.2");
	test(x[0] == 0xD8, "encode_utf16be.seq2.4");
	test(x[1] == 0x00, "encode_utf16be.seq2.3");
	test(x[2] == 0xDC, "encode_utf16be.seq2.6");
	test(x[3] == 0x0A, "encode_utf16be.seq2.5");

	test(encode_utf16be(0x0110000, x, &y) == 1, "encode_utf16be.seq2.7");

	return 0;
}

static int test_length_utf16be(void)
{
	unsigned x;

	TestTitle("*** length_utf16be");
	test(length_utf16be(-1, &x) == 1, "length_utf16be.1");

	test(length_utf16be(0, &x) == 0, "length_utf16be.2");
	test(x == 2, "length_utf16be.3");

	test(length_utf16be('A', &x) == 0, "length_utf16be.4");
	test(x == 2, "length_utf16be.5");

	test(length_utf16be(0xD800, &x) == 1, "length_utf16be.6");

	test(length_utf16be(0x010000, &x) == 0, "length_utf16be.7");
	test(x == 4, "length_utf16be.8");

	test(length_utf16be(0x110000, &x) == 1, "length_utf16be.17");

	return 0;
}


/*****************************************************************************
 *  UTF-32
 *****************************************************************************/
static int test_length_utf32(void)
{
	unsigned x;

	TestTitle("*** length_utf32");
	test(length_utf32(-1, &x) == 1, "length_utf32.1");

	test(length_utf32(0, &x) == 0, "length_utf32.2");
	test(x == 1, "length_utf32.3");

	test(length_utf32('A', &x) == 0, "length_utf32.4");
	test(x == 1, "length_utf32.5");

	test(length_utf32(0xD7FF, &x) == 0, "length_utf32.6");
	test(x == 1, "length_utf32.7");

	test(length_utf32(0xD800, &x) == 1, "length_utf32.8");

	test(length_utf32(0xDFFF, &x) == 1, "length_utf32.9");

	test(length_utf32(0xE000, &x) == 0, "length_utf32.10");
	test(x == 1, "length_utf32.11");

	test(length_utf32(0xFFFF, &x) == 0, "length_utf32.12");
	test(x == 1, "length_utf32.13");

	test(length_utf32(0x010000, &x) == 0, "length_utf32.14");
	test(x == 1, "length_utf32.15");

	test(length_utf32(0x10FFFF, &x) == 0, "length_utf32.16");
	test(x == 1, "length_utf32.17");

	test(length_utf32(0x110000, &x) == 1, "length_utf32.18");

	return 0;
}


/*****************************************************************************
 *  UTF-32LE
 *****************************************************************************/
static int test_init_utf32le(void)
{
	struct state_utf32le x;

	TestTitle("*** init_utf32le");
	memset(&x, 0xBB, sizeof(x));
	init_utf32le(&x);
	test(x.state == 0, "init_utf32le.1");

	return 0;
}

static int test_decode_utf32le(void)
{
	unicode c;
	struct state_utf32le x;

	TestTitle("*** decode_utf32le");
	init_utf32le(&x);
	test(decode_utf32le(&x, 0x32, &c) == 0, "decode_utf32le.1");
	test(decode_utf32le(&x, 0x21, &c) == 0, "decode_utf32le.2");
	test(decode_utf32le(&x, 0x10, &c) == 0, "decode_utf32le.3");
	test(decode_utf32le(&x, 0x00, &c) == 1, "decode_utf32le.4");
	test(c = 0x00102132, "decode_utf32le.5");

	init_utf32le(&x);
	test(decode_utf32le(&x, 0xFF, &c) == 0, "decode_utf32le.6");
	test(decode_utf32le(&x, 0xFF, &c) == 0, "decode_utf32le.7");
	test(decode_utf32le(&x, 0xFF, &c) == 0, "decode_utf32le.8");
	test(decode_utf32le(&x, 0xFF, &c) == -1, "decode_utf32le.9");

	init_utf32le(&x);
	test(decode_utf32le(&x, 0x10, &c) == 0, "decode_utf32le.10");
	test(decode_utf32le(&x, 0x20, &c) == 0, "decode_utf32le.11");
	test(decode_utf32le(&x, 0x30, &c) == 0, "decode_utf32le.12");
	test(decode_utf32le(&x, 0x40, &c) == -1, "decode_utf32le.13");

	init_utf32le(&x);
	test(decode_utf32le(&x, 0x00, &c) == 0, "decode_utf32le.14");
	test(decode_utf32le(&x, 0xD8, &c) == 0, "decode_utf32le.15");
	test(decode_utf32le(&x, 0x00, &c) == 0, "decode_utf32le.16");
	test(decode_utf32le(&x, 0x00, &c) == -1, "decode_utf32le.17");

	return 0;
}

static int test_encode_utf32le(void)
{
	int size;
	uint8_t array[4];

	TestTitle("*** encode_utf32le");
	test(encode_utf32le(-1, array, &size) == 1, "encode_utf32le.1");
	test(encode_utf32le(0, array, &size) == 0, "encode_utf32le.2");
	test(size == 4, "encode_utf32le.3");
	test(array[0] == 0, "encode_utf32le.4");
	test(array[1] == 0, "encode_utf32le.4");
	test(array[2] == 0, "encode_utf32le.4");
	test(array[3] == 0, "encode_utf32le.4");

	test(encode_utf32le(0x00102233, array, &size) == 0, "encode_utf32le.5");
	test(size == 4, "encode_utf32le.6");
	test(array[0] == 0x33, "encode_utf32le.7");
	test(array[1] == 0x22, "encode_utf32le.7");
	test(array[2] == 0x10, "encode_utf32le.7");
	test(array[3] == 0x00, "encode_utf32le.7");

	test(encode_utf32le(0xD7FF, array, &size) == 0, "encode_utf32le.8");
	test(encode_utf32le(0xD800, array, &size) == 1, "encode_utf32le.9");
	test(encode_utf32le(0xDFFF, array, &size) == 1, "encode_utf32le.10");
	test(encode_utf32le(0xE000, array, &size) == 0, "encode_utf32le.11");
	test(encode_utf32le(0x10FFFF, array, &size) == 0, "encode_utf32le.12");
	test(encode_utf32le(0x110000, array, &size) == 1, "encode_utf32le.13");

	return 0;
}

static int test_length_utf32le(void)
{
	unsigned x;

	TestTitle("*** length_utf32le");
	test(length_utf32le(-1, &x) == 1, "length_utf32le.1");

	test(length_utf32le(0, &x) == 0, "length_utf32le.2");
	test(x == 4, "length_utf32le.3");

	test(length_utf32le('A', &x) == 0, "length_utf32le.4");
	test(x == 4, "length_utf32le.5");

	return 0;
}


/*****************************************************************************
 *  UTF-32BE
 *****************************************************************************/
static int test_init_utf32be(void)
{
	struct state_utf32be x;

	TestTitle("*** init_utf32be");
	memset(&x, 0xBB, sizeof(x));
	init_utf32be(&x);
	test(x.state == 0, "init_utf32be.1");

	return 0;
}

static int test_decode_utf32be(void)
{
	unicode c;
	struct state_utf32be x;

	TestTitle("*** decode_utf32be");
	init_utf32be(&x);
	test(decode_utf32be(&x, 0x00, &c) == 0, "decode_utf32be.1");
	test(decode_utf32be(&x, 0x10, &c) == 0, "decode_utf32be.2");
	test(decode_utf32be(&x, 0x21, &c) == 0, "decode_utf32be.3");
	test(decode_utf32be(&x, 0x32, &c) == 1, "decode_utf32be.4");
	test(c = 0x00102132, "decode_utf32be.5");

	init_utf32be(&x);
	test(decode_utf32be(&x, 0xFF, &c) == 0, "decode_utf32be.6");
	test(decode_utf32be(&x, 0xFF, &c) == 0, "decode_utf32be.7");
	test(decode_utf32be(&x, 0xFF, &c) == 0, "decode_utf32be.8");
	test(decode_utf32be(&x, 0xFF, &c) == -1, "decode_utf32be.9");

	init_utf32be(&x);
	test(decode_utf32be(&x, 0x40, &c) == 0, "decode_utf32be.11");
	test(decode_utf32be(&x, 0x30, &c) == 0, "decode_utf32be.12");
	test(decode_utf32be(&x, 0x20, &c) == 0, "decode_utf32be.13");
	test(decode_utf32be(&x, 0x10, &c) == -1, "decode_utf32be.14");

	init_utf32be(&x);
	test(decode_utf32be(&x, 0x00, &c) == 0, "decode_utf32be.15");
	test(decode_utf32be(&x, 0x00, &c) == 0, "decode_utf32be.16");
	test(decode_utf32be(&x, 0xD8, &c) == 0, "decode_utf32be.17");
	test(decode_utf32be(&x, 0x00, &c) == -1, "decode_utf32be.18");

	return 0;
}

static int test_encode_utf32be(void)
{
	int size;
	uint8_t array[4];

	TestTitle("*** encode_utf32be");
	test(encode_utf32be(-1, array, &size) == 1, "encode_utf32be.1");
	test(encode_utf32be(0, array, &size) == 0, "encode_utf32be.2");
	test(size == 4, "encode_utf32be.3");
	test(array[0] == 0, "encode_utf32be.4");
	test(array[1] == 0, "encode_utf32be.4");
	test(array[2] == 0, "encode_utf32be.4");
	test(array[3] == 0, "encode_utf32be.4");

	test(encode_utf32be(0x00102233, array, &size) == 0, "encode_utf32be.5");
	test(size == 4, "encode_utf32be.6");
	test(array[0] == 0x00, "encode_utf32be.7");
	test(array[1] == 0x10, "encode_utf32be.7");
	test(array[2] == 0x22, "encode_utf32be.7");
	test(array[3] == 0x33, "encode_utf32be.7");

	test(encode_utf32be(0xD7FF, array, &size) == 0, "encode_utf32be.8");
	test(encode_utf32be(0xD800, array, &size) == 1, "encode_utf32be.9");
	test(encode_utf32be(0xDFFF, array, &size) == 1, "encode_utf32be.10");
	test(encode_utf32be(0xE000, array, &size) == 0, "encode_utf32be.11");
	test(encode_utf32be(0x10FFFF, array, &size) == 0, "encode_utf32be.12");
	test(encode_utf32be(0x110000, array, &size) == 1, "encode_utf32be.13");

	return 0;
}

static int test_length_utf32be(void)
{
	unsigned x;

	TestTitle("*** length_utf32be");
	test(length_utf32be(-1, &x) == 1, "length_utf32be.1");

	test(length_utf32be(0, &x) == 0, "length_utf32be.2");
	test(x == 4, "length_utf32be.3");

	test(length_utf32be('A', &x) == 0, "length_utf32be.4");
	test(x == 4, "length_utf32be.5");

	return 0;
}


/*****************************************************************************
 *  byte order mark
 *****************************************************************************/
static int test_init_byte_order_mark(void)
{
	uint8_t c;
	struct byte_order_mark x;

	TestTitle("*** init_byte_order_mark");

	init_byte_order_mark(&x);
	test(x.state == 0, "init_byte_order_mark.1");
	test(getc_byte_order_mark(&x, 0, &c) == 1, "init_byte_order_mark.2");

	return 0;
}

static int test_putc_byte_order_mark(void)
{
	uint8_t c;
	enum byte_order_mark_type r, n;
	struct byte_order_mark x;

	TestTitle("*** putc_byte_order_mark");

	/* init */
	init_byte_order_mark(&x);
	n = byte_order_mark_next;
	test(putc_byte_order_mark(&x, 0xEF) == n, "byte_order_mark.init.1");
	test(getc_byte_order_mark(&x, 0, &c) == 0, "byte_order_mark.init.2");
	test(c == 0xEF, "byte_order_mark.init.3");
	test(getc_byte_order_mark(&x, 1, &c) == 1, "byte_order_mark.init.4");

	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.init.5");

	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.init.6");

	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFF) == n, "byte_order_mark.init.7");

	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFF) == n, "byte_order_mark.init.7");

	init_byte_order_mark(&x);
	r = putc_byte_order_mark(&x, 0x2B);
	test(r == byte_order_mark_binary, "byte_order_mark.init.8");

	init_byte_order_mark(&x);
	r = putc_byte_order_mark(&x, 0xAA);
	test(r == byte_order_mark_binary, "byte_order_mark.init.9");
	r = putc_byte_order_mark(&x, 0xBB);
	test(r == byte_order_mark_binary, "byte_order_mark.init.10");

	/* utf8_1 */
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xEF) == n, "byte_order_mark.utf8.1");
	test(putc_byte_order_mark(&x, 0xBB) == n, "byte_order_mark.utf8.2");
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xEF) == n, "byte_order_mark.utf8.3");
	r = putc_byte_order_mark(&x, 0xAA);
	test(r == byte_order_mark_binary, "byte_order_mark.utf8.4");

	/* utf8_2 */
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xEF) == n, "byte_order_mark.utf8.5");
	test(putc_byte_order_mark(&x, 0xBB) == n, "byte_order_mark.utf8.6");
	r = putc_byte_order_mark(&x, 0xBF);
	test(r == byte_order_mark_UTF8, "byte_order_mark.utf8.7");
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xEF) == n, "byte_order_mark.utf8.8");
	test(putc_byte_order_mark(&x, 0xBB) == n, "byte_order_mark.utf8.9");
	r = putc_byte_order_mark(&x, 0xAA);
	test(r == byte_order_mark_binary, "byte_order_mark.utf8.10");

	/* utf16be_1 */
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf16be.1");
	r = putc_byte_order_mark(&x, 0xFF);
	test(r == byte_order_mark_UTF16BE, "byte_order_mark.utf16be.2");
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf16be.3");
	r = putc_byte_order_mark(&x, 0x10);
	test(r == byte_order_mark_binary, "byte_order_mark.utf16be.4");

	/* utf32be_1 */
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.5");
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.6");
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.7");
	r = putc_byte_order_mark(&x, 0x22);
	test(r == byte_order_mark_binary, "byte_order_mark.utf32be.8");

	/* utf32be_2 */
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.9");
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.10");
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf32be.11");
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.12");
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.13");
	r = putc_byte_order_mark(&x, 0x33);
	test(r == byte_order_mark_binary, "byte_order_mark.utf32be.14");

	/* utf32be_3 */
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.15");
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.16");
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf32be.11");
	r = putc_byte_order_mark(&x, 0xFF);
	test(r == byte_order_mark_UTF32BE, "byte_order_mark.utf32be.18");
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.19");
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf32be.20");
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf32be.21");
	r = putc_byte_order_mark(&x, 0x44);
	test(r == byte_order_mark_binary, "byte_order_mark.utf32be.22");

	/* utf16le_utf32le_1 */
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFF) == n, "byte_order_mark.utf16or32.1");
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf16or32.2");
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFF) == n, "byte_order_mark.utf16or32.3");
	r = putc_byte_order_mark(&x, 0x55);
	test(r == byte_order_mark_binary, "byte_order_mark.utf16or32.4");

	/* utf16le_utf32le_2 */
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFF) == n, "byte_order_mark.utf16or32.5");
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf16or32.6");
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf16or32.7");
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFF) == n, "byte_order_mark.utf16or32.8");
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf16or32.9");
	r = putc_byte_order_mark(&x, 0x66);
	test(r == byte_order_mark_UTF16LE, "byte_order_mark.utf16or32.10");

	/* utf16le_utf32le_3 */
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFF) == n, "byte_order_mark.utf16or32.11");
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf16or32.12");
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf16or32.13");
	r = putc_byte_order_mark(&x, 0x00);
	test(r == byte_order_mark_UTF32LE, "byte_order_mark.utf16or32.14");
	init_byte_order_mark(&x);
	test(putc_byte_order_mark(&x, 0xFF) == n, "byte_order_mark.utf16or32.15");
	test(putc_byte_order_mark(&x, 0xFE) == n, "byte_order_mark.utf16or32.16");
	test(putc_byte_order_mark(&x, 0x00) == n, "byte_order_mark.utf16or32.17");
	r = putc_byte_order_mark(&x, 0x77);
	test(r == byte_order_mark_UTF16LE, "byte_order_mark.utf16or32.18");

	return 0;
}

static int test_getc_byte_order_mark(void)
{
	uint8_t c;
	int i, n;
	enum byte_order_mark_type check;
	struct byte_order_mark x;

	TestTitle("*** getc_byte_order_mark");
	init_byte_order_mark(&x);
	putc_byte_order_mark(&x, 0x11);
	putc_byte_order_mark(&x, 0x22);
	putc_byte_order_mark(&x, 0x33);
	putc_byte_order_mark(&x, 0x44);
	test(getc_byte_order_mark(&x, 0, &c) == 0, "getc_byte_order_mark.1");
	test(c == 0x11, "getc_byte_order_mark.2");
	test(getc_byte_order_mark(&x, 3, &c) == 0, "getc_byte_order_mark.3");
	test(c == 0x44, "getc_byte_order_mark.4");
	test(getc_byte_order_mark(&x, 5, &c) == 1, "getc_byte_order_mark.5");
	test(getc_byte_order_mark(&x, -1, &c) == 1, "getc_byte_order_mark.6");

	init_byte_order_mark(&x);
	n = BYTE_ORDER_MARK_HISTORY;
	for (i = 0; i < n; i++)
		check = putc_byte_order_mark(&x, i);
	test(check == byte_order_mark_binary, "getc_byte_order_mark.7");
	test(getc_byte_order_mark(&x, n-1, &c) == 0, "getc_byte_order_mark.8");
	test(c == n-1, "getc_byte_order_mark.9");
	check = putc_byte_order_mark(&x, 0xDD);
	test(check == byte_order_mark_error, "getc_byte_order_mark.10");

	return 0;
}

static int test_result_byte_order_mark(void)
{
	enum byte_order_mark_type check;
	struct byte_order_mark x;

	TestTitle("*** result_byte_order_mark");
	init_byte_order_mark(&x);                                                                 check = result_byte_order_mark(&x);
	test(check == byte_order_mark_next, "result_byte_order_mark.1");

	putc_byte_order_mark(&x, 0x11);
	check = result_byte_order_mark(&x);
	test(check == byte_order_mark_binary, "result_byte_order_mark.1");

	return 0;
}

static int test_sequence_byte_order_mark(void)
{
	struct byte_order_mark x;
	enum byte_order_mark_type check;

	TestTitle("*** sequence_byte_order_mark");

	/* UTF-8 */
	init_byte_order_mark(&x);
	check = putc_byte_order_mark(&x, 0xEF);
	check = putc_byte_order_mark(&x, 0xBB);
	check = putc_byte_order_mark(&x, 0xBF);
	test(check == byte_order_mark_UTF8, "sequence_byte_order_mark.1");

	/* UTF-16BE */
	init_byte_order_mark(&x);
	check = putc_byte_order_mark(&x, 0xFE);
	check = putc_byte_order_mark(&x, 0xFF);
	test(check == byte_order_mark_UTF16BE, "sequence_byte_order_mark.2");

	/* UTF-16LE */
	init_byte_order_mark(&x);
	check = putc_byte_order_mark(&x, 0xFF);
	check = putc_byte_order_mark(&x, 0xFE);
	test(check == byte_order_mark_next, "sequence_byte_order_mark.3");
	check = putc_byte_order_mark(&x, 0x11);
	test(check == byte_order_mark_UTF16LE, "sequence_byte_order_mark.4");

	/* UTF-32BE */
	init_byte_order_mark(&x);
	check = putc_byte_order_mark(&x, 0x00);
	check = putc_byte_order_mark(&x, 0x00);
	check = putc_byte_order_mark(&x, 0xFE);
	check = putc_byte_order_mark(&x, 0xFF);
	test(check == byte_order_mark_UTF32BE, "sequence_byte_order_mark.5");

	/* UTF-32LE */
	init_byte_order_mark(&x);
	check = putc_byte_order_mark(&x, 0xFF);
	check = putc_byte_order_mark(&x, 0xFE);
	check = putc_byte_order_mark(&x, 0x00);
	check = putc_byte_order_mark(&x, 0x00);
	test(check == byte_order_mark_UTF32LE, "sequence_byte_order_mark.6");

	/* UTF-7 (not supported) */
	init_byte_order_mark(&x);
	check = putc_byte_order_mark(&x, 0x2B);
	test(check == byte_order_mark_binary, "sequence_byte_order_mark.7");

	return 0;
}


/*****************************************************************************
 *  stdio
 *****************************************************************************/
#define TEST_DEBUG_FILE		"debug.txt"

/* fgetc */
static int test_fgetc_utf8_write(const void *str, size_t size)
{
	const uint8_t *ptr;
	FILE *fout;
	size_t i;

	fout = fopen(TEST_DEBUG_FILE, "wb");
	if (fout == NULL)
		return 1;
	ptr = (const uint8_t *)str;
	for (i = 0; i < size; i++)
		fputc(ptr[i], fout);
	fclose(fout);

	return 0;
}

static int test_fgetc_utf8_make(const char *str)
{
	FILE *fout;

	fout = fopen(TEST_DEBUG_FILE, "wb");
	if (fout == NULL)
		return 1;
	fprintf(fout, "%s", str);
	fclose(fout);

	return 0;
}

static int test_fgetc_utf8_input(unicode *ret)
{
	int check;
	unicode c;
	FILE *fin;

	fin = fopen(TEST_DEBUG_FILE, "rb");
	if (fin == NULL) {
		*ret = 0;
		return 1;
	}
	c = 0;
	check = fgetc_utf8(fin, &c);
	fclose(fin);

	*ret = c;
	return check;
}

static int test_fgetc_utf8(void)
{
	int check;
	const char *str;
	FILE *fin;
	unicode c;

	TestTitle("*** fgetc_utf8");

	/* EOF */
	str = "";
	test(! test_fgetc_utf8_make(str), "fgetc_utf8.1");
	check = test_fgetc_utf8_input(&c);
	test(check == 0, "fgetc_utf8.2");
	test(c == -1, "fgetc_utf8.3");
	/* 0x20 */
	str = " ";
	test(! test_fgetc_utf8_make(str), "fgetc_utf8.4");
	check = test_fgetc_utf8_input(&c);
	test(check == 0, "fgetc_utf8.5");
	test(c == 0x20, "fgetc_utf8.6");
	/* 0x30 0x31 0x32 */
	str = "\x30\x31\x32";
	test(! test_fgetc_utf8_make(str), "fgetc_utf8.7");
	check = test_fgetc_utf8_input(&c);
	test(check == 0, "fgetc_utf8.8");
	test(c == 0x30, "fgetc_utf8.9");
	/* U+1F019 Mahjong Tile One of Circles */
	str = "\xF0\x9F\x80\x99";
	test(! test_fgetc_utf8_make(str), "fgetc_utf8.10");
	check = test_fgetc_utf8_input(&c);
	test(check == 0, "fgetc_utf8.11");
	test(c == 0x01F019, "fgetc_utf8.12");
	/* Error: 0xE0 0x80 0x80 */
	str = "\xE0\x80\x80\x20";
	test(! test_fgetc_utf8_make(str), "fgetc_utf8.13");
	check = test_fgetc_utf8_input(&c);
	test(check, "fgetc_utf8.14");

	/* 0x00 */
	str = "\x00\x20\x30";
	test(! test_fgetc_utf8_write(str, 3), "fgetc_utf8.15");
	check = test_fgetc_utf8_input(&c);
	test(check == 0, "fgetc_utf8.16");
	test(c == 0x00, "fgetc_utf8.17");

	/* sequence */
	str = "\x20\xF0\x9F\x80\x99\x30";
	test(! test_fgetc_utf8_make(str), "fgetc_utf8.seq.1");
	fin = fopen(TEST_DEBUG_FILE, "rb");
	if (fin == NULL) {
		fprintf(stderr, "fopen error.\n");
		return 1;
	}
	check = fgetc_utf8(fin, &c);
	test(check == 0, "fgetc_utf8.seq.2");
	test(c == 0x20, "fgetc_utf8.seq.3");
	check = fgetc_utf8(fin, &c);
	test(check == 0, "fgetc_utf8.seq.4");
	test(c == 0x01F019, "fgetc_utf8.seq.5");
	check = fgetc_utf8(fin, &c);
	test(check == 0, "fgetc_utf8.seq.6");
	test(c == 0x30, "fgetc_utf8.seq.7");
	check = fgetc_utf8(fin, &c);
	test(check == 0, "fgetc_utf8.seq.8");
	test(c == -1, "fgetc_utf8.seq.9");
	fclose(fin);

	return 0;
}

static int test_fputc_utf8(void)
{
	FILE *file;
	unicode c;

	TestTitle("fputc_utf8");

	file = fopen(TEST_DEBUG_FILE, "wb");
	if (file == NULL)
		return 1;
	test(fputc_utf8(0x10, file) == 0, "fputc_utf8.1");
	test(fputc_utf8(0x1234, file) == 0, "fputc_utf8.2");
	test(fputc_utf8(0x10ABCD, file) == 0, "fputc_utf8.3");
	fclose(file);

	file = fopen(TEST_DEBUG_FILE, "rb");
	if (file == NULL)
		return 1;
	test(fgetc_utf8(file, &c) == 0, "fputc_utf8.4");
	test(c == 0x10, "fputc_utf8.5");
	test(fgetc_utf8(file, &c) == 0, "fputc_utf8.6");
	test(c == 0x1234, "fputc_utf8.7");
	test(fgetc_utf8(file, &c) == 0, "fputc_utf8.8");
	test(c == 0x10ABCD, "fputc_utf8.9");
	test(fgetc_utf8(file, &c) == 0, "fputc_utf8.10");
	test(c == -1, "fputc_utf8.11");
	fclose(file);

	return 0;
}


/*****************************************************************************
 *  memory21
 *****************************************************************************/
static int test_byte_size_memory21(void)
{
	test(byte_size_memory21(0) == 0, "byte_size_memory21.1");
	test(byte_size_memory21(1) == 3, "byte_size_memory21.2");
	test(byte_size_memory21(2) == 6, "byte_size_memory21.3");
	test(byte_size_memory21(10) == 27, "byte_size_memory21.4");

	return 0;
}

static int test_unicode_size_memory21(void)
{
	test(unicode_size_memory21(0) == 0, "unicode_size_memory21.1");
	test(unicode_size_memory21(1) == 0, "unicode_size_memory21.2");
	test(unicode_size_memory21(3) == 1, "unicode_size_memory21.3");
	test(unicode_size_memory21(10) == 3, "unicode_size_memory21.4");

	return 0;
}

static int test_clear_memory21(void)
{
	char x[100];
	unicode c;

	memset(x, 0xAA, 100);
	clear_memory21(x, 10);
	getc_memory21(x, 0, &c);
	test(c == 0, "clear_memory21.1");
	getc_memory21(x, 9, &c);
	test(c == 0, "clear_memory21.2");

	return 0;
}

static int test_getc_memory21(void)
{
	char x[0x010000];
	int i, check;
	unicode a, b;
	uint64_t c;

	/* test.1 */
	memset(x, 0xAA, 0x010000);
	for (i = 0; i < 1000; i++) {
		a = i;
		putc_memory21(x, i, a);
	}
	check = 1;
	for (i = 0; i < 1000; i++) {
		a = i;
		getc_memory21(x, i, &b);
		if (a != b)
			check = 0;
	}
	test(check, "getc_memory21.1");

	/* test.1 */
	memset(x, 0xAA, 0x010000);
	c = 1;
	for (i = 0; i < 1000; i++) {
		c += (123456 * i) >> (i % 11);
		a = (unicode)(c % 0x110000);
		putc_memory21(x, i, a);
	}
	c = 1;
	check = 1;
	for (i = 0; i < 1000; i++) {
		c += (123456 * i) >> (i % 11);
		a = (unicode)(c % 0x110000);
		getc_memory21(x, i, &b);
		if (a != b)
			check = 0;
	}
	test(check, "getc_memory21.2");

	return 0;
}

static int test_compare_memory21(void)
{
	char x[1000], y[1000];

	putc_memory21(x, 0, 10);
	putc_memory21(x, 1, 20);
	putc_memory21(x, 2, 30);
	putc_memory21(y, 0, 10);
	putc_memory21(y, 1, 20);
	putc_memory21(y, 2, 30);
	test(compare_memory21(x, y, 3) == 0, "compare_memory21.1");

	putc_memory21(y, 1, 1111);
	test(compare_memory21(x, y, 3) < 0, "compare_memory21.2");

	putc_memory21(y, 1, 5);
	test(compare_memory21(x, y, 3) > 0, "compare_memory21.3");

	return 0;
}

static int test_equal_memory21(void)
{
	char x[1000], y[1000];
	int i;

	memset(x, 0xAA, 1000);
	memset(y, 0x55, 1000);

	putc_memory21(x, 0, 10);
	putc_memory21(x, 1, 20);
	putc_memory21(x, 2, 30);
	putc_memory21(y, 0, 10);
	putc_memory21(y, 1, 20);
	putc_memory21(y, 2, 30);
	test(equal_memory21(x, y, 3) == 1, "equal_memory21.1");

	putc_memory21(y, 1, 1111);
	test(equal_memory21(x, y, 3) == 0, "equal_memory21.2");

	for (i = 0; i < 8; i++) {
		putc_memory21(x, i, i * 11);
		putc_memory21(y, i, i * 11);
	}
	test(equal_memory21(x, y, 8) == 1, "equal_memory21.3");

	putc_memory21(y, 1, 1111);
	test(equal_memory21(x, y, 3) == 0, "equal_memory21.4");

	return 0;
}


/*****************************************************************************
 *  string21
 *****************************************************************************/
static int test_make_string21(void)
{
	unicode c;
	string21 *ptr;

	ptr = make_string21(10);
	test(ptr, "make_string21.1");
	test(ptr->ptr, "make_string21.2");
	test(ptr->size == 10, "make_string21.3");
	free_string21(ptr);

	ptr = calloc_string21(10);
	test(ptr, "calloc_string21.1");
	test(ptr->ptr, "calloc_string21.2");
	test(ptr->size == 10, "calloc_string21.3");
	getc_memory21(ptr->ptr, 3, &c);
	test(c == 0, "calloc_string21.4");
	free_string21(ptr);

	return 0;
}

static int test_clear_string21(void)
{
	int i;
	unicode c;
	string21 *ptr;

	ptr = make_string21(10);
	for (i = 0; i < 10; i++)
		putc_memory21(ptr->ptr, i, i);
	clear_string21(ptr);
	getc_memory21(ptr->ptr, 0, &c);
	test(c == 0, "clear_memory21.1");
	getc_memory21(ptr->ptr, 9, &c);
	test(c == 0, "clear_memory21.2");
	free_string21(ptr);

	return 0;
}

static int test_getc_string21(void)
{
	unicode c;
	string21 *ptr;

	ptr = make_string21(10);
	putc_memory21(ptr->ptr, 3, 0x1234);
	test(getc_string21(ptr, 3, &c) == 0, "getc_string21.1");
	test(c == 0x1234, "getc_string21.2");

	test(getc_string21(ptr, 11, &c) == 1, "getc_string21.3");
	free_string21(ptr);

	return 0;
}

static int test_putc_string21(void)
{
	unicode c;
	string21 *ptr;

	ptr = make_string21(10);
	test(putc_string21(ptr, 3, 0x5678) == 0, "putc_string21.1");
	test(getc_string21(ptr, 3, &c) == 0, "putc_string21.2");
	test(c == 0x5678, "putc_string21.3");

	test(putc_string21(ptr, 11, 0x20) == 1, "putc_string21.4");
	test(putc_string21(ptr, 3, -1) == 1, "putc_string21.5");
	test(putc_string21(ptr, 3, 0x110000) == 1, "putc_string21.6");
	free_string21(ptr);

	return 0;
}

static int test_compare_string21(void)
{
	string21 *x, *y;

	x = make_string21(3);
	y = make_string21(3);
	putc_string21(x, 0, 10);
	putc_string21(x, 1, 20);
	putc_string21(x, 2, 30);
	putc_string21(y, 0, 10);
	putc_string21(y, 1, 20);
	putc_string21(y, 2, 30);
	test(compare_string21(x, y) == 0, "compare_string21.1");
	putc_string21(y, 2, 40);
	test(compare_string21(x, y) < 0, "compare_string21.2");
	putc_string21(y, 2, 5);
	test(compare_string21(x, y) > 0, "compare_string21.3");
	free_string21(x);
	free_string21(y);

	x = make_string21(3);
	y = make_string21(4);
	putc_string21(x, 0, 10);
	putc_string21(x, 1, 20);
	putc_string21(x, 2, 30);
	putc_string21(y, 0, 10);
	putc_string21(y, 1, 20);
	putc_string21(y, 2, 30);
	putc_string21(y, 4, 40);
	test(compare_string21(x, y) < 0, "compare_string21.4");
	test(compare_string21(y, x) > 0, "compare_string21.5");

	return 0;
}

static int test_equal_string21(void)
{
	string21 *x, *y;

	x = make_string21(3);
	y = make_string21(3);
	putc_string21(x, 0, 10);
	putc_string21(x, 1, 20);
	putc_string21(x, 2, 30);
	putc_string21(y, 0, 10);
	putc_string21(y, 1, 20);
	putc_string21(y, 2, 30);
	test(equal_string21(x, y) == 1, "equal_string21.1");
	putc_string21(y, 2, 40);
	test(equal_string21(x, y) == 0, "equal_string21.2");
	putc_string21(y, 2, 5);
	test(equal_string21(x, y) == 0, "equal_string21.3");
	free_string21(x);
	free_string21(y);

	x = make_string21(3);
	y = make_string21(4);
	putc_string21(x, 0, 10);
	putc_string21(x, 1, 20);
	putc_string21(x, 2, 30);
	putc_string21(y, 0, 10);
	putc_string21(y, 1, 20);
	putc_string21(y, 2, 30);
	putc_string21(y, 4, 40);
	test(equal_string21(x, y) == 0, "equal_string21.4");
	test(equal_string21(y, x) == 0, "equal_string21.5");

	return 0;
}


/*****************************************************************************
 *  main
 *****************************************************************************/
static int main_group(void)
{
	/* UTF-8 */
	TestGroup(test_init_utf8());
	TestGroup(test_decode_utf8());
	TestGroup(test_encode_utf8());
	TestGroup(test_length_utf8());

	/* UTF-16 */
	TestGroup(test_init_utf16());
	TestGroup(test_decode_utf16());
	TestGroup(test_encode_utf16());
	TestGroup(test_length_utf16());

	/* UTF-16LE */
	TestGroup(test_init_utf16le());
	TestGroup(test_decode_utf16le());
	TestGroup(test_encode_utf16le());
	TestGroup(test_length_utf16le());

	/* UTF-16BE */
	TestGroup(test_init_utf16be());
	TestGroup(test_decode_utf16be());
	TestGroup(test_encode_utf16be());
	TestGroup(test_length_utf16be());

	/* UTF-32 */
	TestGroup(test_length_utf32());

	/* UTF-32LE */
	TestGroup(test_init_utf32le());
	TestGroup(test_decode_utf32le());
	TestGroup(test_encode_utf32le());
	TestGroup(test_length_utf32le());

	/* UTF-32BE */
	TestGroup(test_init_utf32be());
	TestGroup(test_decode_utf32be());
	TestGroup(test_encode_utf32be());
	TestGroup(test_length_utf32be());

	/* byte order mark */
	TestGroup(test_init_byte_order_mark());
	TestGroup(test_putc_byte_order_mark());
	TestGroup(test_getc_byte_order_mark());
	TestGroup(test_result_byte_order_mark());
	TestGroup(test_sequence_byte_order_mark());

	/* stdio */
	TestGroup(test_fgetc_utf8());
	TestGroup(test_fputc_utf8());

	/* memory21 */
	TestGroup(test_byte_size_memory21());
	TestGroup(test_unicode_size_memory21());
	TestGroup(test_clear_memory21());
	TestGroup(test_getc_memory21());
	TestGroup(test_compare_memory21());
	TestGroup(test_equal_memory21());

	/* string21 */
	TestGroup(test_make_string21());
	TestGroup(test_clear_string21());
	TestGroup(test_getc_string21());
	TestGroup(test_putc_string21());
	TestGroup(test_compare_string21());
	TestGroup(test_equal_string21());

	printf("OK.\n");
	return 0;
}

int main(void)
{
	int check;

	check = main_group();
	if (check) {
		printf("\n");
		printf("***************\n");
		printf("***  ERROR  ***\n");
		printf("***************\n");
		return 1;
	}

	return 0;
}

