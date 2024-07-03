VPATH = ..:.
source = test.c
object = $(source:.c=.o)
a.out = a.out
CFLAGS = -g -Wall -I. -I..

$(a.out) : $(object)
	$(CC) $(CFLAGS) -o $(a.out) $(object)

.PHONY : clean
clean :
	-rm -f $(a.out) $(object)

