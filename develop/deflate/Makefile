object = \
deflate_encode.o \
deflate_decode.o \
deflate_gzip.o \
deflate_zlib.o \
example_decode.o \
example_encode.o \
example_gzip.o \
example_zlib.o \
main.o

DEBUG =
DEFINE = -O3
#DEBUG = -DHYPD_DEBUG
#DEFINE = -DHYPD_INFO
OPTIONS = -g -pedantic -Wall -I. -Iformat -Iexample
CFLAGS = $(OPTIONS) $(DEFINE) $(DEBUG)
a.out = a.out

$(a.out) : $(object)
	$(CC) $(CFLAGS) -o $(a.out) $(object)

clean :
	-rm -f $(a.out) $(object)

main.o : example/main.c
deflate_encode.o : deflate_encode.c deflate_encode.h
deflate_decode.o : deflate_decode.c deflate_decode.h
deflate_gzip.o : format/deflate_gzip.c format/deflate_gzip.h
deflate_zlib.o : format/deflate_zlib.c format/deflate_zlib.h

example_encode.o : example/example_encode.c example/example_encode.h
example_decode.o : example/example_decode.c example/example_decode.h
example_gzip.o : example/example_gzip.c example/example_gzip.h
example_zlib.o : example/example_zlib.c example/example_zlib.h

