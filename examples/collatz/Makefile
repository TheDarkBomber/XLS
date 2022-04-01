CXX=g++
CC=gcc
XLC=$(abspath ../../build/xls)

LIBXLS=$(abspath ../../lib.cxx)

all: collatz

collatz: main.o lib.o
	$(CC) main.o lib.o -o collatz

main.o: main.xls
	$(XLC) module Collatz\ Sequence out main.o < main.xls

lib.o: $(LIBXLS)
	$(CXX) $(LIBXLS) -c lib.o

clean:
	rm -v *.o
	rm -v collatz
