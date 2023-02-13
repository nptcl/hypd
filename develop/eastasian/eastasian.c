#include <stddef.h>
#include "eastasian.h"

static int eastasian_check(const struct eastasian_struct *str, unicode x)
{
	return (str->a <= x) && (x <= str->b);
}

static enum EastAsianType eastasian_search(unicode x, size_t ai, size_t bi)
{
	size_t ci;
	const struct eastasian_struct *a, *b, *c;

	ci = (ai + bi) / 2;
	a = EastAsianTable + ai;
	b = EastAsianTable + bi;
	c = EastAsianTable + ci;
	if (eastasian_check(a, x))
		return a->c;
	if (eastasian_check(b, x))
		return b->c;
	if (bi <= ai)
		return EastAsian_error;
	if (x < c->a)
		return eastasian_search(x, ai + 1UL, ci);
	else
		return eastasian_search(x, ci, bi - 1UL);
}

enum EastAsianType eastasian_symbol(unicode x)
{
	if (x < 0)
		return EastAsian_error;
	if (x < 0x80)
		return EastAsianAscii[x];
	else
		return eastasian_search(x, 0, EastAsianTable_Size - 1UL);
}

unsigned eastasian_width(unicode x)
{
	return EastAsianSymbol[eastasian_symbol(x)];
}

void init_eastasian(void)
{
	EastAsianSymbol[EastAsian_error] = 0;
	EastAsianSymbol[EastAsian_N] = 1;
	EastAsianSymbol[EastAsian_A] = 2;
	EastAsianSymbol[EastAsian_H] = 1;
	EastAsianSymbol[EastAsian_W] = 2;
	EastAsianSymbol[EastAsian_F] = 2;
	EastAsianSymbol[EastAsian_NA] = 1;
}

