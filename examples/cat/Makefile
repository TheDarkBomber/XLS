CXX=g++
CC=gcc
XLC=$(abspath ../../build/xls)

LIBXLS=$(abspath ../../lib.cxx)

all: cat upcat lowcat

cat: main.o lib.o
	$(CC) main.o lib.o -o cat

upcat: upcat.o lib.o
	$(CC) upcat.o lib.o -o upcat

lowcat: lowcat.o lib.o
	$(CC) lowcat.o lib.o -o lowcat

upcat.o: upcat.xls
	$(XLC) module UPCAT\ PROGRAM out upcat.o < upcat.xls

lowcat.o: lowcat.xls
	$(XLC) module lowcat\ program out lowcat.o < lowcat.xls

main.o: main.xls
	$(XLC) module Cat\ Program out main.o < main.xls

lib.o: $(LIBXLS)
	$(CXX) $(LIBXLS) -c lib.o

clean:
	rm -v *.o
	rm -v *cat
