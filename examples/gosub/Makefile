CC=gcc
XLC=$(abspath ../../build/xls)

all: gosub

gosub: main.o
	$(CC) main.o -o gosub

main.o: main.xls
	$(XLC) module Gosub out main.o < main.xls

clean:
	rm -v *.o
	rm -v gosub
