#ifndef __EXAMPLE_ZLIB_HEADER__
#define __EXAMPLE_ZLIB_HEADER__

#include <stdio.h>

int zlib_decode_FILE(FILE *fin, FILE *fout);
int zlib_encode_FILE(FILE *fin, FILE *fout);

#endif

