#include "Type.hxx"
#include "Parser.hxx"
#include "Variables.hxx"
#include <llvm/IR/Constants.h>

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

#define castret(value) do {casted = value; TypeAnnotation[casted] = type; return casted; } while(0)
#define elc(x) llvm::ElementCount::get(x, false)

SSA* Cast(XLSType type, SSA* toCast) {
	XLSType toCastType = GetType(toCast);
	if (type == toCastType) return toCast;
	SSA* casted;
	if (!type.UID || !toCastType.UID) castret(llvm::PoisonValue::get(type.Type));
	if (type.Type->isVoidTy() || toCast->getType()->isVoidTy()) castret(llvm::Constant::getNullValue(type.Type));
	if (type.IsPointer && toCastType.IsRangedPointer) castret(Builder->CreateExtractValue(toCast, llvm::ArrayRef<unsigned>(RANGED_POINTER_VALUE), "xls_rptp_cast"));
	if (type.IsRangedPointer && toCastType.IsPointer) {
	    SSA* baseStruct = ZeroSSA(type);
	    baseStruct = Builder->CreateInsertValue(baseStruct, toCast, llvm::ArrayRef<unsigned>(RANGED_POINTER_VALUE), "xls_ptrp_cast");
	    baseStruct = Builder->CreateInsertValue(baseStruct, llvm::ConstantInt::get(DefinedTypes["#addrsize"].Type, llvm::APInt(DefinedTypes["#addrsize"].Size, 1, false)), llvm::ArrayRef<unsigned>(RANGED_POINTER_COUNTOF), "xls_initialise_rp");
	    TypeAnnotation[baseStruct] = type;
	    return baseStruct;
	}
	if (type.IsRangedPointer || toCastType.IsRangedPointer) castret(Builder->CreateBitCast(toCast, type.Type, "xls_rp_bitcast"));
	if (type.IsPointer && toCastType.IsPointer) castret(Builder->CreateBitCast(toCast, type.Type, "xls_ptp_cast"));
	if (type.IsPointer) castret(Builder->CreateIntToPtr(toCast, type.Type, "xls_itp_cast"));
	if (toCastType.IsPointer) castret(Builder->CreatePtrToInt(toCast, type.Type, "xls_pti_cast"));
	if (type.IsStruct) castret(Builder->CreateBitCast(toCast, type.Type, "xls_bitcast"));
	if (type.IsVector && !toCastType.IsVector) {
		if (toCastType.Size == type.Size) castret(Builder->CreateBitCast(toCast, type.Type));
		if (toCastType.UID == DefinedTypes[type.Dereference].UID) castret(Builder->CreateVectorSplat(elc(type.Length), toCast));
		return ZeroSSA(type);
	}
	if (toCastType.IsVector && !type.IsVector) {
		if (toCastType.Size == type.Size) castret(Builder->CreateBitCast(toCast, type.Type));
		return ZeroSSA(type);
	}
	if (toCastType.IsVector && type.IsVector) {
		if (toCastType.Size == type.Size) castret(Builder->CreateBitCast(toCast, type.Type));
		return ZeroSSA(type);
	}
	if (type.Signed) castret(Builder->CreateSExtOrTrunc(toCast, type.Type, "xls_simplicit_cast"));
	castret(Builder->CreateZExtOrTrunc(toCast, type.Type, "xls_implicit_cast"));
}

SSA* BroadCast(XLSType type, SSA* toCast) {
	XLSType toCastType = GetType(toCast);
	XLSType eltype = DefinedTypes[type.Dereference];
	SSA* casted;
	if (eltype.UID == toCastType.UID) castret(Builder->CreateVectorSplat(elc(type.Length), toCast));
	castret(Builder->CreateVectorSplat(elc(type.Length), Cast(eltype, toCast)));
}

#undef castret

XLSType GetType(SSA* toType) {
	if (TypeAnnotation.find(toType) == TypeAnnotation.end())
	    TypeAnnotation[toType] = TypeMap[toType->getType()];
	return TypeAnnotation[toType];
}
