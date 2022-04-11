BUILD?=build
ASM=nasm
ASMFLAGS=-f elf64
LD=ld
LDFLAGS=-shared

XLC=$(BUILD)/xls
XLCFLAGS=module NXLib
XPP=$(BUILD)/xpp

ASM_SOURCES=$(wildcard *.asm)
ASM_OBJECTS=$(patsubst %.asm, $(OBJ_LOCATION)/%.a64, $(ASM_SOURCES))
XLS_SOURCES=$(wildcard *.xls)
XLS_OBJECTS=$(patsubst %.xls, $(OBJ_LOCATION)/%.o, $(XLS_SOURCES))

ASM_LINKED_OBJECTS=$(subst $(OBJ_LOCATION)/xrt.a64,, $(ASM_OBJECTS))
OBJ_LOCATION=$(BUILD)/nxlib

all: nxlib

nxlib: $(BUILD)/nxlib.so $(XLC) $(XPP)

$(BUILD)/nxlib.so: $(ASM_OBJECTS) $(XLS_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $(ASM_LINKED_OBJECTS) $(XLS_OBJECTS)
	mv -v $(OBJ_LOCATION)/xrt.a64 $(BUILD)/xrt.o
	@echo "[NXLIB] COMPILED nxlib.so"

$(OBJ_LOCATION)/%.a64: %.asm always
	mkdir -pv $(@D)
	$(ASM) $(ASMFLAGS) -o $@ $<
	@echo "[NXLIB] COMPILED " $<

$(OBJ_LOCATION)/%.o: %.xls always
	mkdir -pv $(@D)
	$(XPP) < $< | $(XLC) $(XLCFLAGS) out $@
	@echo "[NXLIB] COMPILED "

always:
	mkdir -pv $(OBJ_LOCATION)

clean:
	rm -fv $(BUILD)/nxlib.so
	rm -fv $(BUILD)/xrt.o
	rm -fv $(OBJ_LOCATION)/*
