#include "Parser.hxx"
#include "Lexer.hxx"
#include "JIT.hxx"
#include "colours.def.h"
#include <llvm/ADT/APInt.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
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
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>
#include <map>
#include <ctype.h>
#include <stdio.h>

static llvm::ExitOnError ExitIfError;

ParserFlags Flags;

Token CurrentToken;
Token GetNextToken() { return CurrentToken = GetToken(); }

std::map<char, Precedence> BinaryPrecedence;

UQP(llvm::LLVMContext) GlobalContext;
UQP(llvm::IRBuilder<>) Builder;
UQP(llvm::Module) GlobalModule;
UQP(llvm::legacy::FunctionPassManager) GlobalFPM;
UQP(JITCompiler) GlobalJIT;

std::map<std::string, UQP(SignatureNode)> FunctionSignatures;

std::map<std::string, Alloca*> AllonymousValues;

void AlertError(const char *error) { llvm::errs() << COLOUR_RED << "Error: " << error << COLOUR_END << "\n"; }
void AlertWarning(const char *warning) { llvm::errs() << COLOUR_PURPLE << "Warning: " << warning << COLOUR_END << "\n"; }

#define DEFERROR(flag) { flag = 1; AlertError(error); return nullptr; }
#define DEFWARNING(flag) { flag = 1; AlertWaring(warning); return nullptr; }

UQP(Expression) ParseError(const char* error) DEFERROR(Flags.ParseError);
UQP(SignatureNode) ParseError(const char* error, void*) DEFERROR(Flags.ParseError);
SSA *CodeError(const char* error) DEFERROR(Flags.CodeError);

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
	case LEXEME_IF:
		return ParseIf();
	case LEXEME_ELSE:
		return nullptr;
	case LEXEME_DWORD_VARIABLE:
		return ParseDwordDeclaration();
	default:
		switch (CurrentToken.Value) {
		case '(': // ')'
			return ParseParenthetical();
		case '{': // '}'
			return ParseBlock();
		default: return ParseError("Unknown token value.");
		}
	}
}

Precedence GetTokenPrecedence() {
	if (!isascii(CurrentToken.Value)) return PRECEDENCE_INVALID;
	Precedence precedence = BinaryPrecedence[CurrentToken.Value];
	return precedence;
}

UQP(Expression) ParseExpression() {
	UQP(Expression) LHS = ParseUnary();
	if (!LHS) return nullptr;
	return ParseBinary(PRECEDENCE_INVALID, std::move(LHS));
}

UQP(Expression) ParseBinary(Precedence precedence, UQP(Expression) LHS) {
	for(;;) {
		Precedence tokenPrecedence = GetTokenPrecedence();
		if (tokenPrecedence <= precedence) return LHS;

		char binaryOperator = CurrentToken.Value;
		GetNextToken();

		UQP(Expression) RHS = ParseUnary();
		if (!RHS) return nullptr;

		Precedence nextPrecedence = GetTokenPrecedence();
		if (tokenPrecedence < nextPrecedence) {
			RHS = ParseBinary((Precedence)(tokenPrecedence + 1), std::move(RHS));
			if (!RHS) return nullptr;
		}

		LHS = MUQ(BinaryExpression, binaryOperator, std::move(LHS), std::move(RHS));
	}
}

UQP(Expression) ParseUnary() {
	if (!BinaryPrecedence[CurrentToken.Value]) return ParseDispatcher();

	char operator_ = CurrentToken.Value;
	GetNextToken();
	printf("Operator = %c, Current = %c\n", operator_, CurrentToken.Value);
	if (UQP(Expression) operand = ParseUnary()) {
		printf("Return\n");
		return MUQ(UnaryExpression, operator_, std::move(operand));
	}
	return nullptr;
}

UQP(Expression) ParseBlock() {
	GetNextToken();
	// '{'
	std::vector<UQP(Expression)> expressions;
	if (CurrentToken.Value != '}') {
		for (;;) {
			if (UQP(Expression) expression = ParseExpression())
				expressions.push_back(std::move(expression));
			else return nullptr;
			// '{'
			if (CurrentToken.Value == '}') break;
			if (CurrentToken.Value != ';') return ParseError("Expected semicolon.");
			GetNextToken();
		}
	}
	GetNextToken();
	return MUQ(BlockExpression, std::move(expressions));
}

UQP(Expression) ParseIf() {
	GetNextToken();
	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis in if condition.");
	// ')'
	UQP(Expression) condition = ParseParenthetical();
	if (!condition) return nullptr;

	UQP(Expression) thenBranch = ParseExpression();
	if (!thenBranch) return nullptr;

	UQP(Expression) elseBranch;
	if (CurrentToken.Type == LEXEME_ELSE) {
		GetNextToken();
		elseBranch = ParseExpression();
		if (!elseBranch) return nullptr;
	} else elseBranch = MUQ(DwordExpression, 0);

	return MUQ(IfExpression, std::move(condition), std::move(thenBranch), std::move(elseBranch));
}

UQP(Expression) ParseDwordDeclaration() {
	GetNextToken();
	VDX(std::string, UQP(Expression)) variableNames;
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier after DWORD declaration.");

	for (;;) {
		std::string name = CurrentIdentifier;
		GetNextToken();

		UQP(Expression) definer;
		if (CurrentToken.Value == '=') {
			GetNextToken();
			definer = ParseExpression();
			if (!definer) return nullptr;
		}

		variableNames.push_back(std::make_pair(name, std::move(definer)));
		if (CurrentToken.Value != ',') break;
		GetNextToken();

		if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected more identifiers after DWORD declaration.");
	}

	if (CurrentToken.Value != ';') return ParseError("Expected end of DWORD declaration. (insert semicolon)");
	GetNextToken();
	UQP(Expression) body = ParseExpression();
	if (!body) return nullptr;

	return MUQ(DwordDeclarationExpression, std::move(variableNames), std::move(body));
}

UQP(SignatureNode) ParseOperatorSignature() {
	if (CurrentIdentifier != "unary" && CurrentIdentifier != "binary") return ParseError("Expected operator type, unary or binary, or standard declaration/implementation of non-binary function.", nullptr);

	SignatureType signatureType = CurrentIdentifier == "unary" ? SIGNATURE_UNARY : SIGNATURE_BINARY;
	Precedence precedence = PRECEDENCE_USER_DEFAULT;

	std::string functionName = "#op::" + CurrentIdentifier + "::#" + GetNextToken().Value;
	GetNextToken();

	if (CurrentToken.Type == LEXEME_INTEGER) {
		if (CurrentInteger == 0) return ParseError("Invalid precedence: must be greater than zero.", nullptr);
		precedence = (Precedence)CurrentInteger;
		GetNextToken();
	}

	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis in operator prototype.", nullptr);

	std::vector<std::string> argumentNames;
	while (GetNextToken().Type == LEXEME_IDENTIFIER)
		argumentNames.push_back(CurrentIdentifier);
	if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis in operator prototype.", nullptr);
	GetNextToken();

	if (signatureType && argumentNames.size() != signatureType) return ParseError("Invalid number of operands for operator.", nullptr);
	return MUQ(SignatureNode, functionName, std::move(argumentNames), llvm::CallingConv::Fast, true, precedence);
}

UQP(SignatureNode) ParseSignature() {
	if (CurrentToken.Type != LEXEME_IDENTIFIER)
		return ParseError("Expected function name in function signature", nullptr);

	llvm::CallingConv::ID convention = llvm::CallingConv::C;
	if (CurrentToken.Subtype == LEXEME_CALLING_CONVENTION) {
		if (!std::string("cdecl").compare(CurrentIdentifier))
			convention = llvm::CallingConv::C;
		else if (!std::string("fastcc").compare(CurrentIdentifier))
			convention = llvm::CallingConv::Fast;
		else if (!std::string("coldcc").compare(CurrentIdentifier))
			convention = llvm::CallingConv::Cold;
		else if (!std::string("tailcc").compare(CurrentIdentifier))
			convention = llvm::CallingConv::Tail;
		else if (!std::string("webkitjscc").compare(CurrentIdentifier))
			convention = llvm::CallingConv::WebKit_JS;
		else if (!std::string("win64cc").compare(CurrentIdentifier))
			convention = llvm::CallingConv::Win64;

		GetNextToken();
		if (CurrentToken.Type != LEXEME_IDENTIFIER)
			return ParseError("Expected function name in function signature", nullptr);
	}
	std::string functionName = CurrentIdentifier;
	GetNextToken();

	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis in function prototype", nullptr);

	std::vector<std::string> argumentNames;
	while (GetNextToken().Type == LEXEME_IDENTIFIER)
		argumentNames.push_back(CurrentIdentifier);
	if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis in function prototype.", nullptr);
	GetNextToken();

	return MUQ(SignatureNode, functionName, std::move(argumentNames), convention);
}

UQP(FunctionNode) ParseImplementation() {
	GetNextToken();
	UQP(SignatureNode) signature = ParseSignature();
	if (!signature) return nullptr;

	if (UQP(Expression) expression = ParseExpression())
		return MUQ(FunctionNode, std::move(signature), std::move(expression));
	return nullptr;
}

UQP(FunctionNode) ParseOperatorDefinition() {
	GetNextToken();
	UQP(SignatureNode) signature = ParseOperatorSignature();
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

Alloca *createEntryBlockAlloca(llvm::Function *function, llvm::StringRef variableName) {
	llvm::IRBuilder<> apiobuilder(&function->getEntryBlock(), function->getEntryBlock().begin());
	return apiobuilder.CreateAlloca(llvm::Type::getInt32Ty(*GlobalContext), nullptr, variableName);
}

SSA *DwordExpression::Render() {
	return llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, Value, false));
}

SSA *VariableExpression::Render() {
	Alloca *value = AllonymousValues[Name];
	if (!value) CodeError("Reference to undeclared variable.");
	return Builder->CreateLoad(value->getAllocatedType(), value, Name.c_str());
}

SSA *BinaryExpression::Render() {
	if (Operator == '=') {
		VariableExpression *LAssignment = static_cast<VariableExpression*>(LHS.get());
		if (!LAssignment) return CodeError("Assignment on fire.");
		SSA *value = RHS->Render();
		if (!value) return nullptr;
		SSA *variable = AllonymousValues[LAssignment->GetName()];
		if (!variable) return CodeError("Unknown variable name.");
		Builder->CreateStore(value, variable);
		return value;
	}

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
		break;
	}

	llvm::Function *function = getFunction(std::string("#op::binary::#") + Operator);
	if (!function) return CodeError("Unknown binary operator.");
	SSA *Operands[2] = { left, right };
	return Builder->CreateCall(function, Operands, "xls_binary_operation");
}

SSA *UnaryExpression::Render() {
	SSA *operand = Operand->Render();
	if (!operand) return nullptr;
	llvm::Function *function = getFunction(std::string("#op::unary::#") + Operator);
	if (!function) return CodeError("Unknown unary operator");
	return Builder->CreateCall(function, operand, "xls_unary_operation");
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

	llvm::CallInst* callInstance = Builder->CreateCall(called, ArgumentVector, "xls_call");
	callInstance->setCallingConv(called->getCallingConv());
	return callInstance;
}

SSA *BlockExpression::Render() {
	std::vector<SSA*> ExpressionVector;
	for (uint i = 0, e = Expressions.size(); i != e; i++) {
		ExpressionVector.push_back(Expressions[i]->Render());
		if (!ExpressionVector.back()) return nullptr;
	}

	return ExpressionVector.back();
}

SSA *IfExpression::Render() {
	SSA *condition = Condition->Render();
	if (!condition) return nullptr;

	if (condition->getType() != llvm::Type::getInt1Ty(*GlobalContext))
		condition = Builder->CreateICmpNE(condition, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false)), "xls_if_condition");
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_then", function);
	llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_else");
	llvm::BasicBlock *afterBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_after_if");
	Builder->CreateCondBr(condition, thenBlock, elseBlock);

	Builder->SetInsertPoint(thenBlock);
	SSA *thenBranch = ThenBranch->Render();
	if (!thenBranch) return nullptr;

	Builder->CreateBr(afterBlock);
	thenBlock = Builder->GetInsertBlock();

	function->getBasicBlockList().push_back(elseBlock);
	Builder->SetInsertPoint(elseBlock);

	SSA *elseBranch = ElseBranch->Render();
	if (!elseBranch) return nullptr;

	Builder->CreateBr(afterBlock);
	elseBlock = Builder->GetInsertBlock();

	function->getBasicBlockList().push_back(afterBlock);
	Builder->SetInsertPoint(afterBlock);
	llvm::PHINode *phiNode = Builder->CreatePHI(llvm::Type::getInt32Ty(*GlobalContext), 2, "xls_if_block");
	phiNode->addIncoming(thenBranch, thenBlock);
	phiNode->addIncoming(elseBranch, elseBlock);
	return phiNode;
}

SSA *DwordDeclarationExpression::Render() {
	std::vector<Alloca*> Bindings;
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	for (uint i = 0, e = VariableNames.size(); i != e; i++) {
		const std::string &variableName = VariableNames[i].first;
		Expression *definer = VariableNames[i].second.get();

		SSA *definerSSA;
		if (definer) {
			definerSSA = definer->Render();
			if (!definerSSA) return nullptr;
		} else definerSSA = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false));

		Alloca *alloca = createEntryBlockAlloca(function, variableName);
		Builder->CreateStore(definerSSA, alloca);

		Bindings.push_back(AllonymousValues[variableName]);
		AllonymousValues[variableName] = alloca;
	}
	SSA *body = Body->Render();
	if (!body) return nullptr;

	for (uint i = 0, e = VariableNames.size(); i != e; i++)
		AllonymousValues[VariableNames[i].first] = Bindings[i];

	return body;
}

llvm::Function *SignatureNode::Render() {
	std::vector<llvm::Type*> DwordType(Arguments.size(), llvm::Type::getInt32Ty(*GlobalContext));

	llvm::FunctionType *functionType = llvm::FunctionType::get(llvm::Type::getInt32Ty(*GlobalContext), DwordType, false);

	llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, Name, GlobalModule.get());
	function->setCallingConv(Convention);

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

	if (signature.Binary()) BinaryPrecedence[signature.GetOperatorName()] = signature.GetOperatorPrecedence();
	if (signature.Unary()) BinaryPrecedence[signature.GetOperatorName()] = PRECEDENCE_UNPRECEDENTED;

  llvm::BasicBlock *basicBlock = llvm::BasicBlock::Create(*GlobalContext, "entry", function);
  Builder->SetInsertPoint(basicBlock);

  AllonymousValues.clear();
  for (llvm::Argument &argument : function->args()) {
		Alloca *alloca = createEntryBlockAlloca(function, argument.getName());
		Builder->CreateStore(&argument, alloca);
		AllonymousValues[std::string(argument.getName())] = alloca;
	}

  if (SSA *returnValue = Body->Render()) {
    Builder->CreateRet(returnValue);
    llvm::verifyFunction(*function);
    GlobalFPM->run(*function);
    return function;
	}

	function->eraseFromParent();
	return nullptr;
}

void InitialiseModule(std::string moduleName) {
	GlobalContext = MUQ(llvm::LLVMContext);
	GlobalModule = MUQ(llvm::Module, moduleName, *GlobalContext);
	GlobalFPM = MUQ(llvm::legacy::FunctionPassManager, GlobalModule.get());

	GlobalFPM->add(llvm::createPromoteMemoryToRegisterPass());
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

	GlobalFPM->add(llvm::createPromoteMemoryToRegisterPass());
  GlobalFPM->add(llvm::createInstructionCombiningPass());
  GlobalFPM->add(llvm::createReassociatePass());
  GlobalFPM->add(llvm::createGVNPass());
  GlobalFPM->add(llvm::createCFGSimplificationPass());

  GlobalFPM->doInitialization();
}

void HandleImplementation() {
	if (UQP(FunctionNode) functionNode = ParseImplementation()) {
		if (llvm::Function* functionIR = functionNode->Render()) {
			// fprintf(stderr, "Generated IR:\n");
			// functionIR->print(llvm::errs());
			// fprintf(stderr, "\n");
			if (Flags.EmitIRToSTDOUT) functionIR->print(llvm::outs());
			// ExitIfError(GlobalJIT->AddModule(llvm::orc::ThreadSafeModule(std::move(GlobalModule), std::move(GlobalContext))));
			// InitialiseJIT();
		}
	} else GetNextToken();
}

void HandleOperatorDefinition() {
  if (UQP(FunctionNode) functionNode = ParseOperatorDefinition()) {
    if (llvm::Function *functionIR = functionNode->Render()) {
      // fprintf(stderr, "Generated IR:\n");
      // functionIR->print(llvm::errs());
      // fprintf(stderr, "\n");
			if (Flags.EmitIRToSTDOUT) functionIR->print(llvm::outs());
      // ExitIfError(GlobalJIT->AddModule(llvm::orc::ThreadSafeModule(std::move(GlobalModule), std::move(GlobalContext))));
      // InitialiseJIT();
    }
  } else GetNextToken();
}

void HandleExtern() {
	if (UQP(SignatureNode) signature = ParseExtern()) {
		if (llvm::Function* signatureIR = signature->Render()) {
			// fprintf(stderr, "Generated IR:\n");
			// signatureIR->print(llvm::errs());
			// fprintf(stderr, "\n");
			if (Flags.EmitIRToSTDOUT) signatureIR->print(llvm::outs());
			FunctionSignatures[signature->GetName()] = std::move(signature);
		}
	} else GetNextToken();
}

void HandleUnboundedExpression() {
	if (UQP(FunctionNode) functionNode = ParseUnboundedExpression()) {
		functionNode->Render();
		// if (llvm::Function* functionIR = functionNode->Render()) {
		// 	// llvm::IntrusiveRefCntPtr<llvm::orc::ResourceTracker> RT = GlobalJIT->GetMainJITDylib().createResourceTracker();
		// 	// llvm::orc::ThreadSafeModule TSM = llvm::orc::ThreadSafeModule(std::move(GlobalModule), std::move(GlobalContext));

		// 	// ExitIfError(GlobalJIT->AddModule(std::move(TSM), RT));
		// 	// InitialiseJIT();

		// 	// fprintf(stderr, "Executing IR:\n");
		// 	// functionIR->print(llvm::errs());
		// 	// fprintf(stderr, "\n");

		// 	llvm::JITEvaluatedSymbol symbol = ExitIfError(GlobalJIT->Lookup("__mistakeman"));

		// 	dword (*I32)() = (dword (*)())(intp)symbol.getAddress();
		// 	fprintf(stderr, "Evaluated to %u\n", I32());

		// 	ExitIfError(RT->remove());
		// }
	} else GetNextToken();
}


extern "C" dword putchard(dword X) {
	fputc((char)X, stderr);
	return 0;
}

extern "C" dword printd(dword X) {
	fprintf(stderr, "%u\n", X);
	return 0;
}
