object = base64.o
CFLAGS = -g -pedantic -Wall -I..
VPATH = ..
a.out = a.out

$(a.out) : example.o $(object)
	$(CC) $(CFLAGS) -o $(a.out) example.o $(object)

clean :
	-rm -f $(a.out) example.o $(object)

