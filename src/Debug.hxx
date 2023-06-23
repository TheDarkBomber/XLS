#ifndef __DEBUG_XLS_H_
#define __DEBUG_XLS_H_
#include <llvm/IR/Metadata.h>
#include <llvm/IR/DIBuilder.h>
#include <map>
#include "Type.hxx"
#include "macros.def.h"

struct DebugInfo {
	llvm::DICompileUnit* CompileUnit;
	UQP(llvm::DIBuilder) Builder;
	std::vector<llvm::DIScope*> Blocks;
	std::map<dword, llvm::DIType*> CachedTypes;
	llvm::DIType* GetType(XLSType type);
	llvm::DISubroutineType* GetFunctionType(SignatureNode signature);
	void EmitLocation(llvm::IRBuilder<>* IRBuilder, Expression* expr = nullptr);
};

extern DebugInfo Dbg;

#define EMIT_DEBUG if (Flags.Debug) Dbg.EmitLocation(Builder.get(), this)

#endif
