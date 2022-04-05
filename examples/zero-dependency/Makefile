ASM=nasm
ASMFLAGS=-f elf64
LD=ld
XLC=$(abspath ../../build/xls)

all: hello

hello: main.o lib.o
	$(LD) lib.o main.o -o hello

main.o: main.xls
	$(XLC) module Zero\ Dependency out main.o < main.xls

lib.o: lib.asm
	$(ASM) $(ASMFLAGS) lib.asm -o lib.o

clean:
	rm -v *.o
	rm -v hello
