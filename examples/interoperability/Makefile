CXX=g++
CC=gcc
XLC=$(abspath ../../build/xls)

LIBXLS=$(abspath ../../lib.cxx)

all: interoperability

interoperability: main.o tribonacci.o lib.o
	$(CC) main.o tribonacci.o lib.o -o tribonacci

main.o: main.xls
	$(XLC) module Interoperability out main.o < main.xls

tribonacci.o: tribonacci.xls
	$(XLC) module Interoperability out tribonacci.o < tribonacci.xls

lib.o: $(LIBXLS)
	$(CXX) $(LIBXLS) -c lib.o

clean:
	rm -v *.o
	rm -v tribonacci
