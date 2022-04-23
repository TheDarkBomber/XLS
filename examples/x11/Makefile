CC=gcc
CCFLAGS=-lX11
XLC=$(abspath ../../build/xls)

all: graphical

graphical: main.o
	$(CC) $(CCFLAGS) main.o -o graphical

main.o: main.xls
	$(XLC) module X11\ Basic\ Window out main.o < main.xls

clean:
	rm -v *.o
	rm -v graphical
