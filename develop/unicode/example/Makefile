object = unicode.o main.o

#DEBUG =
#DEFINE = -O3
DEBUG = -DHYPD_DEBUG
DEFINE =
OPTIONS = -g -pedantic -Wall
CFLAGS = $(OPTIONS) $(DEFINE) $(DEBUG) -I.. -I.
a.out = a.out

$(a.out) : $(object)
	$(CC) $(CFLAGS) -o $(a.out) $(object)

clean :
	-rm -f $(a.out) $(object)

unicode.o : ../unicode.c

