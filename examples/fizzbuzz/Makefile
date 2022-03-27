CXX=g++
CC=gcc
XLC=$(abspath ../../build/xls)

LIBXLS=$(abspath ../../lib.cxx)

all: fizzbuzz

fizzbuzz: main.o lib.o
	$(CC) main.o lib.o -o fizzbuzz

main.o: main.xls
	$(XLC) module Fizzbuzz out main.o < main.xls

lib.o: $(LIBXLS)
	$(CXX) $(LIBXLS) -c lib.o

clean:
	rm -v *.o
	rm -v fizzbuzz
