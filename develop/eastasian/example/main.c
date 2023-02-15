#include "eastasian.h"
#include <stdio.h>

int main(void)
{
	unsigned x;

	/* ASCII A */
	x = eastasian_width('A');
	printf("A: %u\n", x);

	/* HIRAGANA LETTER A */
	x = eastasian_width(0x3042);
	printf("HIRAGANA LETTER A: %u\n", x);

	/* Error */
	x = eastasian_width(0x8FFFFFFF);
	printf("Error: %u\n", x);

	/* Change */
	EastAsianSymbol[EastAsian_error] = 0;
	EastAsianSymbol[EastAsian_N] = 100;
	EastAsianSymbol[EastAsian_A] = 200;
	EastAsianSymbol[EastAsian_H] = 300;
	EastAsianSymbol[EastAsian_W] = 400;
	EastAsianSymbol[EastAsian_F] = 500;
	EastAsianSymbol[EastAsian_Na] = 600;

	/* 00A9;N           # So         COPYRIGHT SIGN */
	x = eastasian_width(0x00A9);
	printf("N: %u\n", x);

	/* 00A1;A           # Po         INVERTED EXCLAMATION MARK */
	x = eastasian_width(0x00A1);
	printf("A: %u\n", x);

	/* FF61;H           # Po         HALFWIDTH IDEOGRAPHIC FULL STOP */
	x = eastasian_width(0xFF61);
	printf("H: %u\n", x);

	/* 2329;W           # Ps         LEFT-POINTING ANGLE BRACKET */
	x = eastasian_width(0x2329);
	printf("W: %u\n", x);

	/* FF0A;F           # Po         FULLWIDTH ASTERISK */
	x = eastasian_width(0xFF0A);
	printf("F: %u\n", x);

	/* 0020;Na          # Zs         SPACE */
	x = eastasian_width(0x0020);
	printf("Na: %u\n", x);

	/* 1F100..1F10A;A   # No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA */
	x = eastasian_width(0x1F100);
	printf("A: %u\n", x);
	x = eastasian_width(0x1F105);
	printf("A: %u\n", x);
	x = eastasian_width(0x1F10A);
	printf("A: %u\n", x);
	x = eastasian_width(0x1F10B);
	printf("N: %u\n", x);

	return 0;
}

