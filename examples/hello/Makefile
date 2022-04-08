CC=gcc
XLC=$(abspath ../../build/xls)

all: hello

hello: main.o
	$(CC) main.o -o hello

main.o: main.xls
	$(XLC) module Hello out main.o < main.xls

clean:
	rm -v *.o
	rm -v hello
