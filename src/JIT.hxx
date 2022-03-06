#ifndef __JIT_XLS_H_
#define __JIT_XLS_H_
#include "macros.def.h"
#include <llvm-c/Target.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/Mangling.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/LLVMContext.h>
#include <memory>

class JITCompiler {
private:
	UQP(llvm::orc::ExecutionSession) session;
	llvm::DataLayout layout;
	llvm::orc::MangleAndInterner mangler;
	llvm::orc::RTDyldObjectLinkingLayer objectLayer;
	llvm::orc::IRCompileLayer compileLayer;
	llvm::orc::JITDylib &mainJD;
public:
  JITCompiler(UQP(llvm::orc::ExecutionSession) _session, llvm::orc::JITTargetMachineBuilder jtmb, llvm::DataLayout _layout)
		: session(std::move(_session)),
			layout(std::move(_layout)),
			mangler(*this->session, this->layout),
			objectLayer(*this->session, []() {
					return MUQ(llvm::SectionMemoryManager);
				}),
			compileLayer(*this->session, objectLayer, std::make_unique<llvm::orc::ConcurrentIRCompiler>(std::move(jtmb))),
			mainJD(this->session->createBareJITDylib("<main>")) {
		mainJD.addGenerator(llvm::cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(layout.getGlobalPrefix())));
	}

	~JITCompiler() {
		if (llvm::Error error = session->endSession())
			session->reportError(std::move(error));
	}

	static llvm::Expected<UQP(JITCompiler)> Create() {
		LLVMInitializeNativeTarget();
		LLVMInitializeX86TargetInfo();
		LLVMInitializeX86Target();
		LLVMInitializeX86TargetMC();
		LLVMInitializeNativeAsmPrinter();
		LLVMInitializeNativeAsmParser();
		llvm::Expected<UQP(llvm::orc::SelfExecutorProcessControl)> executorProcessControl = llvm::orc::SelfExecutorProcessControl::Create();
		if (!executorProcessControl) return executorProcessControl.takeError();

		UQP(llvm::orc::ExecutionSession) executionSession = MUQ(llvm::orc::ExecutionSession, std::move(*executorProcessControl));

		llvm::orc::JITTargetMachineBuilder JTMB(executionSession->getExecutorProcessControl().getTargetTriple());

		llvm::Expected<llvm::DataLayout> layout = JTMB.getDefaultDataLayoutForTarget();
		if (!layout) return layout.takeError();

		return MUQ(JITCompiler, std::move(executionSession), std::move(JTMB), std::move(*layout));
	}

	const llvm::DataLayout &GetDataLayout() const { return layout; }
	llvm::orc::JITDylib &GetMainJITDylib() { return mainJD; }

	llvm::Error AddModule(llvm::orc::ThreadSafeModule TSM, llvm::orc::ResourceTrackerSP RTSP = nullptr) {
		if (!RTSP) RTSP = mainJD.getDefaultResourceTracker();
		return compileLayer.add(RTSP, std::move(TSM));
	}

	llvm::Expected<llvm::JITEvaluatedSymbol> Lookup(llvm::StringRef name) {
		return session->lookup({&mainJD}, mangler(name.str()));
	}
};

#endif
