CC=gcc
XLC=$(abspath ../../build/xls)

all: fizzbuzz

fizzbuzz: main.o
	$(CC) main.o -o fizzbuzz

main.o: main.xls
	$(XLC) module Macro-based\ Fizzbuzz out main.o < main.xls

clean:
	rm -v *.o
	rm -v fizzbuzz
