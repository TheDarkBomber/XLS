#include "Type.hxx"
#include "Parser.hxx"

std::map<std::string, XLSType> DefinedTypes;
std::map<llvm::Type*, XLSType> TypeMap;

std::map<SSA*, XLSType> TypeAnnotation;
std::map<llvm::Function*, XLSType> ReturnTypeAnnotation;
std::map<llvm::Argument*, XLSType> ArgumentTypeAnnotation;

bool operator==(XLSType A, XLSType B) { return A.Name == B.Name; }

bool operator<(XLSType A, XLSType B) { return A.Size < B.Size; }

bool operator<=(XLSType A, XLSType B) { return A.Size <= B.Size; }

bool operator<(SDX(XLSType, std::string) A, SDX(XLSType, std::string) B) {
  return A.first.Size < B.first.Size;
}

bool operator<=(SDX(XLSType, std::string) A, SDX(XLSType, std::string) B) {
  return A.first.Size <= B.first.Size;
}

dword GetCountof(XLSType type) {
	if (type.Size == 0) return 0;
	if (type.IsStruct) {
		dword total = 0;
		for (auto value : type.Structure.Fields) {
			total += GetCountof(value.second.second);
		}
		return total;
	}
	return 1;
}

SSA* Cast(XLSType type, SSA *toCast) {
  if (type == GetType(toCast)) return toCast;
  SSA* casted;
  TypeAnnotation[casted] = type;
  if (!type.UID || !GetType(toCast).UID) return casted = llvm::PoisonValue::get(type.Type);
  if (type.Type->isVoidTy() || toCast->getType()->isVoidTy()) return casted = llvm::Constant::getNullValue(type.Type);
  if (type.IsPointer && toCast->getType()->isPointerTy()) return casted = Builder->CreateBitCast(toCast, type.Type, "xls_ptp_cast");
  if (type.IsPointer) return casted = Builder->CreateIntToPtr(toCast, type.Type, "xls_itp_cast");
  if (toCast->getType()->isPointerTy()) return casted = Builder->CreatePtrToInt(toCast, type.Type, "xls_pti_cast");
  if (type.IsStruct) return casted = Builder->CreateBitCast(toCast, type.Type, "xls_bitcast");
  if (type.Signed) return casted = Builder->CreateSExtOrTrunc(toCast, type.Type, "xls_simplicit_cast");
  casted = Builder->CreateZExtOrTrunc(toCast, type.Type, "xls_implicit_cast");
  return casted;
}

XLSType GetType(SSA* toType) {
  if (TypeAnnotation.find(toType) == TypeAnnotation.end())
    TypeAnnotation[toType] = TypeMap[toType->getType()];
  return TypeAnnotation[toType];
}
