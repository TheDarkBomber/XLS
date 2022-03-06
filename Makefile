BUILD=build
SOURCES=src

CXX=g++
# CXXFLAGS=-march=x86-64 -mtune=generic -O2 -pipe -fno-plt -fexceptions -Wp,-D_FORTIFY_SOURCE=2 -Wformat -Werror=format-security -fstack-clash-protection -fcf-protection -flto -ffat-lto-objects -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -D_THREAD_SAFE -D_REENTRANT
CXXFLAGS=$(shell llvm-config --cxxflags --ldflags --system-libs --libs core)
CXXFLAGS+= -rdynamic
LD=$(CXX)
LDFLAGS=$(CXXFLAGS)

CXX_SOURCES=$(wildcard $(SOURCES)/*.cxx)
CXX_OBJECTS=$(patsubst $(SOURCES)/%.cxx, $(OBJ_LOCATION)/%.o, $(CXX_SOURCES))
OBJ_LOCATION=$(BUILD)

all: xls

xls: $(BUILD)/xls

$(BUILD)/xls: $(CXX_OBJECTS)
	@echo $(CXX_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^ $(CCLIBRARIES)
	@echo "[XLS] COMPILED xls"

$(OBJ_LOCATION)/%.o: $(SOURCES)/%.cxx always
	mkdir -pv $(@D)
	$(CXX) $(CXXFLAGS) -c -o $@ $<
	@echo "[XLS] COMPILED " $<

always:
	mkdir -pv $(OBJ_LOCATION)

clean:
	rm -fv $(BUILD)/xls
	rm -fv $(OBJ_LOCATION)/*
