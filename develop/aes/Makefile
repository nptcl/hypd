source = main.c aes.c
object = $(source:.c=.o)
a.out = a.out
CFLAGS = -g -Wall

$(a.out) : $(object)
	$(CC) $(CFLAGS) -o $(a.out) $(object)

.PHONY : clean
clean :
	-rm -f $(a.out) $(object)

