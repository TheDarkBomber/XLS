CC=gcc
XLC=$(abspath ../../build/xls)

LIBXLS=$(abspath ../../lib.cxx)

all: cat upcat lowcat

cat: main.o
	$(CC) main.o -o cat

upcat: upcat.o
	$(CC) upcat.o -o upcat

lowcat: lowcat.o
	$(CC) lowcat.o -o lowcat

upcat.o: upcat.xls
	$(XLC) module UPCAT\ PROGRAM out upcat.o < upcat.xls

lowcat.o: lowcat.xls
	$(XLC) module lowcat\ program out lowcat.o < lowcat.xls

main.o: main.xls
	$(XLC) module Cat\ Program out main.o < main.xls

clean:
	rm -v *.o
	rm -v *cat
