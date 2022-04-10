BUILD=build
SOURCES=src

CXX=g++
CXXFLAGS=$(shell llvm-config --cxxflags --ldflags --system-libs --libs core)
CXXFLAGS+= -rdynamic
LD=$(CXX)
LDFLAGS=$(CXXFLAGS)

CXX_SOURCES=$(wildcard $(SOURCES)/*.cxx)
CXX_OBJECTS=$(patsubst $(SOURCES)/%.cxx, $(OBJ_LOCATION)/%.o, $(CXX_SOURCES))
OBJ_LOCATION=$(BUILD)

all: xls

nxlib: $(BUILD)/nxlib.so

$(BUILD)/nxlib.so: always
	$(MAKE) -C $(SOURCES)/nxlib BUILD=$(abspath $(BUILD))

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
	rm -rfv $(OBJ_LOCATION)/*
