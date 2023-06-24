#include "Debug.hxx"
#include <llvm/ADT/None.h>
#include <llvm/IR/DebugInfoMetadata.h>

DebugInfo Dbg;

llvm::DIType* DebugInfo::GetType(XLSType type) {
	if (CachedTypes.find(type.UID) != CachedTypes.end()) return CachedTypes[type.UID];

	llvm::DIType* R;
	if (type.IsPointer && !type.IsFP) {
		R = Builder->createPointerType(GetType(DefinedTypes[type.Dereference]), type.Size, type.Size, llvm::None, type.Name);
	} else if (type.IsRangedPointer) {
		auto types = std::vector<llvm::Metadata*>(2);
		types[0] = Builder->createMemberType(CompileUnit->getScope(), "value", CompileUnit->getFile(), 0, GlobalLayout->getPointerSizeInBits(), GlobalLayout->getPointerSizeInBits(), 0, llvm::DINode::FlagPublic, GetType(DefinedTypes[type.Dereference + "*"]));
		types[1] = Builder->createMemberType(CompileUnit->getScope(), "count", CompileUnit->getFile(), 0, GlobalLayout->getPointerSizeInBits(), GlobalLayout->getPointerSizeInBits(), GlobalLayout->getPointerSizeInBits(), llvm::DINode::FlagPublic, GetType(DefinedTypes["#addrsize"]));
		R = Builder->createStructType(CompileUnit->getScope(), type.Name, CompileUnit->getFile(), 0, 2 * GlobalLayout->getPointerSizeInBits(), GlobalLayout->getPointerSizeInBits(), llvm::DINode::FlagPublic, Builder->createUnspecifiedType("<unspecified>"), Builder->getOrCreateArray(types));
	} else if (type.IsStruct) {
		std::vector<llvm::Metadata*> types;
		for (auto F : type.Structure.Fields) {
			types.push_back(Builder->createMemberType(CompileUnit->getScope(), F.first, CompileUnit->getFile(), 0, F.second.second.Size, 0, type.Structure.Layout->getElementOffsetInBits(F.second.first), llvm::DINode::FlagPublic, GetType(F.second.second)));
		}
		R = Builder->createStructType(CompileUnit->getScope(), type.Name, CompileUnit->getFile(), 0, type.Size, type.Structure.Layout->getAlignment().value(), llvm::DINode::FlagPublic, Builder->createUnspecifiedType("<unspecified>"), Builder->getOrCreateArray(llvm::ArrayRef<llvm::Metadata*>(types)));
	} else if (type.IsFP) {
		std::vector<llvm::Metadata*> types;
		for (auto X : type.FPData) {
			types.push_back(GetType(X));
		}
		R = Builder->createSubroutineType(Builder->getOrCreateTypeArray(types));
	} else if (type.Signed) {
		R = Builder->createBasicType(type.Name, type.Size, llvm::dwarf::DW_ATE_signed);
	} else {
		R = Builder->createBasicType(type.Name, type.Size, llvm::dwarf::DW_ATE_unsigned);
	}

	CachedTypes[type.UID] = R;
	return R;
}

llvm::DISubroutineType* DebugInfo::GetFunctionType(SignatureNode signature) {
	std::vector<llvm::Metadata*> types;
	types.push_back(GetType(signature.GetType()));

	for (auto X : signature.GetArguments()) {
		types.push_back(GetType(X.second));
	}

	return Builder->createSubroutineType(Builder->getOrCreateTypeArray(types));
}

void DebugInfo::EmitLocation(llvm::IRBuilder<>* IRBuilder, Expression* expr) {
	if (!expr) return IRBuilder->SetCurrentDebugLocation(llvm::DebugLoc());
	llvm::DIScope* scope;
	if (Blocks.empty()) scope = CompileUnit;
	else scope = Blocks.back();
	IRBuilder->SetCurrentDebugLocation(llvm::DILocation::get(scope->getContext(), expr->GetRow(), expr->GetColumn(), scope));
}
