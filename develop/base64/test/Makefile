object = base64.o
CFLAGS = -g -pedantic -Wall -I..
VPATH = ..
a.out = a.out

$(a.out) : test.o $(object)
	$(CC) $(CFLAGS) -o $(a.out) test.o $(object)

clean :
	-rm -f $(a.out) test.o $(object)

