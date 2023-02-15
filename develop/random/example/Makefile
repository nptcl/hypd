object = main.o md5encode.o random.o

#DEBUG =
#DEFINE = -O3
DEBUG = -DHYPD_DEBUG
DEFINE =
OPTIONS = -g -pedantic -Wall -I. -I.. -DHYPD_FREEBSD
CFLAGS = $(OPTIONS) $(DEFINE) $(DEBUG)
a.out = a.out

$(a.out) : $(object)
	$(CC) $(CFLAGS) -o $(a.out) $(object)

clean :
	-rm -f $(a.out) $(object)

md5encode.o : ../md5encode.c
random.o : ../random.c

