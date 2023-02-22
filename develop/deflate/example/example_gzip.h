#ifndef __EXAMPLE_GZIP_HEADER__
#define __EXAMPLE_GZIP_HEADER__

#include <stdio.h>

int gzip_decode_FILE(FILE *fin, FILE *fout);
int gzip_encode_FILE(FILE *fin, FILE *fout);

#endif

