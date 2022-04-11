BUILD?=build
CC=gcc
CCFLAGS=

C_SOURCES=$(wildcard *.c)
C_OBJECTS=$(patsubst %.c, $(OBJ_LOCATION)/%.o, $(C_SOURCES))
OBJ_LOCATION=$(BUILD)/preprocessor

all: preprocessor

preprocessor: $(BUILD)/xpp

$(BUILD)/xpp: $(C_OBJECTS)
	$(CC) -o $@ $^
	@echo "[XPP] COMPILED xpp"

$(OBJ_LOCATION)/%.o: %.c always
	mkdir -pv $(@D)
	$(CC) $(CCFLAGS) -c -o $@ $<
	@echo "[XPP] COMPILED " $<

always:
	mkdir -pv $(OBJ_LOCATION)

clean:
	rm -fv $(BUILD)/xpp
	rm -fv $(OBJ_LOCATION)/*
