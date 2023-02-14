object = eastasian.o main.o

#DEBUG =
#DEFINE = -O3
DEBUG = -DHYPD_DEBUG
DEFINE =
OPTIONS = -g -pedantic -Wall -I.. -I.
CFLAGS = $(OPTIONS) $(DEFINE) $(DEBUG)
a.out = a.out

$(a.out) : $(object)
	$(CC) $(CFLAGS) -o $(a.out) $(object)

clean :
	-rm -f $(a.out) $(object)

eastasian.o : ../eastasian.c

