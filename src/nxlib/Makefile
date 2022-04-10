BUILD?=build
ASM=nasm
ASMFLAGS=-f elf64
LD=ld
LDFLAGS=-shared

ASM_SOURCES=$(wildcard *.asm)
ASM_OBJECTS=$(patsubst %.asm, $(OBJ_LOCATION)/%.a64, $(ASM_SOURCES))
ASM_LINKED_OBJECTS=$(subst $(OBJ_LOCATION)/xrt.a64,, $(ASM_OBJECTS))
OBJ_LOCATION=$(BUILD)/nxlib

all: nxlib

nxlib: $(BUILD)/nxlib.so

$(BUILD)/nxlib.so: $(ASM_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $(ASM_LINKED_OBJECTS)
	mv -v $(OBJ_LOCATION)/xrt.a64 $(BUILD)/xrt.o
	@echo "[NXLIB] COMPILED nxlib.so"

$(OBJ_LOCATION)/%.a64: %.asm always
	mkdir -pv $(@D)
	$(ASM) $(ASMFLAGS) -o $@ $<
	@echo "[NXLIB] COMPILED " $<

always:
	mkdir -pv $(OBJ_LOCATION)

clean:
	rm -fv $(BUILD)/nxlib.so
	rm -fv $(BUILD)/xrt.o
	rm -fv $(OBJ_LOCATION)/*
