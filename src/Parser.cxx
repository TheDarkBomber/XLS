#include "Parser.hxx"
#include "Lexer.hxx"
#include "JIT.hxx"
#include <llvm/ADT/APInt.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <map>
#include <ctype.h>
#include <stdio.h>

static llvm::ExitOnError ExitIfError;

Token CurrentToken;
Token GetNextToken() { return CurrentToken = GetToken(); }

std::map<char, Precedence> BinaryPrecedence;

UQP(llvm::LLVMContext) GlobalContext;
UQP(llvm::IRBuilder<>) Builder;
UQP(llvm::Module) GlobalModule;
UQP(llvm::legacy::FunctionPassManager) GlobalFPM;
UQP(JITCompiler) GlobalJIT;

std::map<std::string, UQP(SignatureNode)> FunctionSignatures;

std::map<std::string, SSA*> AllonymousValues;

void AlertError(const char *error) { fprintf(stderr, "Error: %s\n", error); }

#define DEFERROR { AlertError(error); return nullptr; }

UQP(Expression) ParseError(const char* error) DEFERROR;
UQP(SignatureNode) ParseError(const char* error, void*) DEFERROR;
SSA *CodeError(const char* error) DEFERROR;

UQP(Expression) ParseDwordExpression() {
	MDU(DwordExpression) result = MUQ(DwordExpression, CurrentInteger);
	GetNextToken();
	return std::move(result);
}

UQP(Expression) ParseParenthetical() {
	GetNextToken();
	UQP(Expression) inside = ParseExpression();
	if (!inside) return nullptr;
	// '('
	if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis.");
	GetNextToken();
	return inside;
}

UQP(Expression) ParseIdentifier() {
	std::string identifier = CurrentIdentifier;
	GetNextToken();
	if (CurrentToken.Value != '(') return MUQ(VariableExpression, identifier);

	GetNextToken();
	std::vector<UQP(Expression)> arguments;
	if (CurrentToken.Value != ')') {
		for(;;) {
			if (UQP(Expression) argument = ParseExpression())
				arguments.push_back(std::move(argument));
			else return nullptr;

			// '('
			if (CurrentToken.Value == ')') break;

			if (CurrentToken.Value != ',')
				return ParseError("Expected close parenthesis or comma in argument list.");

			GetNextToken();
		}
	}

	GetNextToken();
	return MUQ(CallExpression, identifier, std::move(arguments));
}

UQP(Expression) ParseDispatcher() {
	switch (CurrentToken.Type) {
	case LEXEME_IDENTIFIER:
		return ParseIdentifier();
	case LEXEME_INTEGER:
		return ParseDwordExpression();
	default:
		switch (CurrentToken.Value) {
		case '(': // ')'
			return ParseParenthetical();
		default: return ParseError("Unknown token value.");
		}
	}
}

Precedence GetTokenPrecedence() {
	if (!isascii(CurrentToken.Value)) return PRECEDENCE_INVALID;
	Precedence precedence = BinaryPrecedence[CurrentToken.Value];
	if (precedence <= PRECEDENCE_COMPARE) return PRECEDENCE_INVALID;
	return precedence;
}

UQP(Expression) ParseExpression() {
	UQP(Expression) LHS = ParseDispatcher();
	if (!LHS) return nullptr;
	return ParseBinary(PRECEDENCE_INVALID, std::move(LHS));
}

UQP(Expression) ParseBinary(Precedence precedence, UQP(Expression) LHS) {
	for(;;) {
		Precedence tokenPrecedence = GetTokenPrecedence();
		if (tokenPrecedence <= precedence) return LHS;

		char binaryOperator = CurrentToken.Value;
		GetNextToken();

		UQP(Expression) RHS = ParseDispatcher();
		if (!RHS) return nullptr;

		Precedence nextPrecedence = GetTokenPrecedence();
		if (tokenPrecedence < nextPrecedence) {
			RHS = ParseBinary((Precedence)(tokenPrecedence + 1), std::move(RHS));
			if (!RHS) return nullptr;
		}

		LHS = MUQ(BinaryExpression, binaryOperator, std::move(LHS), std::move(RHS));
	}
}

UQP(SignatureNode) ParseSignature() {
	if (CurrentToken.Type != LEXEME_IDENTIFIER)
		return ParseError("Expected function name in function signature", nullptr);

	std::string functionName = CurrentIdentifier;
	GetNextToken();

	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis in function prototype", nullptr);

	std::vector<std::string> argumentNames;
	while (GetNextToken().Type == LEXEME_IDENTIFIER)
		argumentNames.push_back(CurrentIdentifier);
	if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis in function prototype.", nullptr);
	GetNextToken();

	return MUQ(SignatureNode, functionName, std::move(argumentNames));
}

UQP(FunctionNode) ParseImplementation() {
	GetNextToken();
	UQP(SignatureNode) signature = ParseSignature();
	if (!signature) return nullptr;

	if (UQP(Expression) expression = ParseExpression())
		return MUQ(FunctionNode, std::move(signature), std::move(expression));
	return nullptr;
}

UQP(SignatureNode) ParseExtern() {
	GetNextToken();
	return ParseSignature();
}

UQP(FunctionNode) ParseUnboundedExpression() {
	if (UQP(Expression) expression = ParseExpression()) {
		MDU(SignatureNode) signature = MUQ(SignatureNode, "__mistakeman", std::vector<std::string>());
		return MUQ(FunctionNode, std::move(signature), std::move(expression));
	}
	return nullptr;
}

llvm::Function *getFunction(std::string name) {
	if (llvm::Function *function = GlobalModule->getFunction(name)) return function;

	std::_Rb_tree_iterator<std::pair<const std::basic_string<char>, UQP(SignatureNode)>> signature = FunctionSignatures.find(name);
	if (signature != FunctionSignatures.end()) return signature->second->Render();

	return nullptr;
}

SSA *DwordExpression::Render() {
	return llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, Value, false));
}

SSA *VariableExpression::Render() {
	SSA *value = AllonymousValues[Name];
	if (!value) CodeError("Reference to undeclared variable.");
	return value;
}

SSA *BinaryExpression::Render() {
	SSA *left = LHS->Render();
	SSA *right = RHS->Render();
	if (!left || !right) return nullptr;
	switch (Operator) {
	case '+':
		return Builder->CreateAdd(left, right, "xls_add");
	case '-':
		return Builder->CreateSub(left, right, "xls_subtract");
	case '*':
		return Builder->CreateMul(left, right, "xls_multiply");
	case '<':
		return Builder->CreateICmpULT(left, right, "xls_lt_compare");
	case '>':
		return Builder->CreateICmpUGT(left, right, "xls_gt_compare");
	default:
		return CodeError("Unknown binary operator.");
	}
}

SSA *CallExpression::Render() {
	llvm::Function *called = getFunction(Called);
	if (!called) return CodeError("Call to undeclared function.");

	if (called->arg_size() != Arguments.size()) return CodeError("Argument call size mismatch with real argument size.");

	std::vector<SSA*> ArgumentVector;
	for (uint i = 0, e = Arguments.size(); i != e; i++) {
		ArgumentVector.push_back(Arguments[i]->Render());
		if (!ArgumentVector.back()) return nullptr;
	}

	return Builder->CreateCall(called, ArgumentVector, "xls_call");
}

llvm::Function *SignatureNode::Render() {
	std::vector<llvm::Type*> DwordType(Arguments.size(), llvm::Type::getInt32Ty(*GlobalContext));

	llvm::FunctionType *functionType = llvm::FunctionType::get(llvm::Type::getInt32Ty(*GlobalContext), DwordType, false);

	llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, Name, GlobalModule.get());

	uint index = 0;
	for (llvm::Argument &argument : function->args())
		argument.setName(Arguments[index++]);

	return function;
}

llvm::Function *FunctionNode::Render() {
  SignatureNode &signature = *Signature;
	FunctionSignatures[Signature->GetName()] = std::move(Signature);
	llvm::Function *function = getFunction(signature.GetName());
	if (!function) return nullptr;

  llvm::BasicBlock *basicBlock = llvm::BasicBlock::Create(*GlobalContext, "entry", function);
  Builder->SetInsertPoint(basicBlock);

  AllonymousValues.clear();
  for (llvm::Argument &argument : function->args())
    AllonymousValues[std::string(argument.getName())] = &argument;

  if (SSA *returnValue = Body->Render()) {
    Builder->CreateRet(returnValue);
    llvm::verifyFunction(*function);
    GlobalFPM->run(*function);
    return function;
	}

	function->eraseFromParent();
	return nullptr;
}

void InitialiseModule() {
	GlobalContext = MUQ(llvm::LLVMContext);
	GlobalModule = MUQ(llvm::Module, "XLS Module", *GlobalContext);
	GlobalFPM = MUQ(llvm::legacy::FunctionPassManager, GlobalModule.get());

	GlobalFPM->add(llvm::createInstructionCombiningPass());
	GlobalFPM->add(llvm::createReassociatePass());
	GlobalFPM->add(llvm::createGVNPass());
	GlobalFPM->add(llvm::createCFGSimplificationPass());

	GlobalFPM->doInitialization();

	Builder = MUQ(llvm::IRBuilder<>, *GlobalContext);
}

void PreinitialiseJIT() {
	GlobalJIT = ExitIfError(JITCompiler::Create());
}

void InitialiseJIT() {
  GlobalContext = MUQ(llvm::LLVMContext);
  GlobalModule = MUQ(llvm::Module, "XLS-JIT", *GlobalContext);
	GlobalModule->setDataLayout(GlobalJIT->GetDataLayout());
  GlobalFPM = MUQ(llvm::legacy::FunctionPassManager, GlobalModule.get());

  Builder = MUQ(llvm::IRBuilder<>, *GlobalContext);

  GlobalFPM->add(llvm::createInstructionCombiningPass());
  GlobalFPM->add(llvm::createReassociatePass());
  GlobalFPM->add(llvm::createGVNPass());
  GlobalFPM->add(llvm::createCFGSimplificationPass());

  GlobalFPM->doInitialization();
}

void HandleImplementation() {
	if (UQP(FunctionNode) functionNode = ParseImplementation()) {
		if (llvm::Function* functionIR = functionNode->Render()) {
			fprintf(stderr, "Generated IR:\n");
			functionIR->print(llvm::errs());
			fprintf(stderr, "\n");
			ExitIfError(GlobalJIT->AddModule(llvm::orc::ThreadSafeModule(std::move(GlobalModule), std::move(GlobalContext))));
			InitialiseJIT();
		}
	} else GetNextToken();
}

void HandleExtern() {
	if (UQP(SignatureNode) signature = ParseExtern()) {
		if (llvm::Function* signatureIR = signature->Render()) {
			fprintf(stderr, "Generated IR:\n");
			signatureIR->print(llvm::errs());
			fprintf(stderr, "\n");
			FunctionSignatures[signature->GetName()] = std::move(signature);
		}
	} else GetNextToken();
}

void HandleUnboundedExpression() {
	if (UQP(FunctionNode) functionNode = ParseUnboundedExpression()) {
		if (llvm::Function* functionIR = functionNode->Render()) {
			llvm::IntrusiveRefCntPtr<llvm::orc::ResourceTracker> RT = GlobalJIT->GetMainJITDylib().createResourceTracker();
			llvm::orc::ThreadSafeModule TSM = llvm::orc::ThreadSafeModule(std::move(GlobalModule), std::move(GlobalContext));

			ExitIfError(GlobalJIT->AddModule(std::move(TSM), RT));
			InitialiseJIT();

			fprintf(stderr, "Executing IR:\n");
			functionIR->print(llvm::errs());
			fprintf(stderr, "\n");

			llvm::JITEvaluatedSymbol symbol = ExitIfError(GlobalJIT->Lookup("__mistakeman"));

			dword (*I32)() = (dword (*)())(intp)symbol.getAddress();
			fprintf(stderr, "Evaluated to %u\n", I32());

			ExitIfError(RT->remove());
		}
	} else GetNextToken();
}


extern "C" dword putchard(dword X) {
	fputc((char)X, stderr);
	return 0;
}
