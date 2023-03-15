#ifndef __TYPE_XLS_H_
#define __TYPE_XLS_H_
#include "Parser.hxx"

enum RangedPointerField {
	RANGED_POINTER_VALUE = 0,
	RANGED_POINTER_COUNTOF = 1
};

extern std::map<std::string, XLSType> DefinedTypes;
extern std::map<llvm::Type*, XLSType> TypeMap;

extern std::map<SSA*, XLSType> TypeAnnotation;
extern std::map<llvm::Function*, XLSType> ReturnTypeAnnotation;
extern std::map<llvm::Argument*, XLSType> ArgumentTypeAnnotation;

bool operator==(XLSType A, XLSType B);
bool operator<(XLSType A, XLSType B);
bool operator<=(XLSType A, XLSType B);
bool operator<(SDX(XLSType, std::string) A, SDX(XLSType, std::string) B);
bool operator<=(SDX(XLSType, std::string) A, SDX(XLSType, std::string) B);

dword GetCountof(XLSType type);
SSA* Cast(XLSType type, SSA *toCast);
XLSType GetType(SSA* toType);

#endif
