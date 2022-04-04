#include "Parser.hxx"
#include "Lexer.hxx"
#include "JIT.hxx"
#include "colours.def.h"
#include <llvm/ADT/APInt.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
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
#include <string>

static llvm::ExitOnError ExitIfError;

ParserFlags Flags;

Token CurrentToken;
Token GetNextToken() { return CurrentToken = GetToken(); }

std::map<std::string, Precedence> BinaryPrecedence;

UQP(llvm::LLVMContext) GlobalContext;
UQP(llvm::IRBuilder<>) Builder;
UQP(llvm::Module) GlobalModule;
UQP(llvm::legacy::FunctionPassManager) GlobalFPM;
UQP(JITCompiler) GlobalJIT;

std::map<std::string, XLSType> DefinedTypes;
std::map<llvm::Type*, XLSType> TypeMap;

std::map<std::string, UQP(SignatureNode)> FunctionSignatures;
std::map<std::string, llvm::BasicBlock*> AllonymousLabels;

std::map<std::string, AnnotatedValue> AllonymousValues;
std::map<std::string, AnnotatedGlobal> GlobalValues;

void AlertError(const char *error) { llvm::errs() << COLOUR_RED << "Error: " << error << COLOUR_END << "\n"; }
void AlertWarning(const char *warning) { llvm::errs() << COLOUR_PURPLE << "Warning: " << warning << COLOUR_END << "\n"; }

#define DEFERROR(flag) { flag = 1; AlertError(error); return nullptr; }
#define DEFWARNING(flag) { flag = 1; AlertWaring(warning); return nullptr; }

UQP(Expression) ParseError(const char* error) DEFERROR(Flags.ParseError);
UQP(SignatureNode) ParseError(const char* error, void*) DEFERROR(Flags.ParseError);
UQP(Statement) ParseError(const char* error, void*, void*) DEFERROR(Flags.ParseError);
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

UQP(Expression) ParseIdentifier(bool isVolatile) {
	std::string identifier = CurrentIdentifier;
	GetNextToken();
	if (CurrentToken.Value != '(') return MUQ(VariableExpression, identifier, isVolatile);

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
	case LEXEME_WHILE:
		if (CurrentToken.Subtype == LEXEME_ELSE) return ParseWhile(true);
		return ParseWhile();
	case LEXEME_LABEL:
		return ParseLabel();
	case LEXEME_JUMP:
		return ParseJump();
	case LEXEME_ELSE:
		return nullptr;
	case LEXEME_DWORD_VARIABLE:
		return ParseDeclaration(DefinedTypes["dword"]);
	case LEXEME_WORD_VARIABLE:
		return ParseDeclaration(DefinedTypes["word"]);
	case LEXEME_BYTE_VARIABLE:
		return ParseDeclaration(DefinedTypes["byte"]);
	case LEXEME_BOOLE_VARIABLE:
		return ParseDeclaration(DefinedTypes["boole"]);
	case LEXEME_SIZEOF:
		return ParseSizeof();
	case LEXEME_VOLATILE:
		GetNextToken();
		return ParseExpression(true);
	default:
		switch (CurrentToken.Value) {
		case '(': // ')'
			return ParseParenthetical();
		case '{': // '}'
			return ParseBlock();
		default: PrintToken(CurrentToken); return ParseError("Unknown token value.");
		}
	}
}

Precedence GetTokenPrecedence() {
	if (BinaryPrecedence.find(CurrentOperator) == BinaryPrecedence.end())
		return PRECEDENCE_INVALID;
	Precedence precedence = BinaryPrecedence[CurrentOperator];
	return precedence;
}

UQP(Expression) ParseExpression(bool isVolatile) {
	UQP(Expression) LHS = ParseUnary();
	if (!LHS) return nullptr;
	return ParseBinary(PRECEDENCE_INVALID, std::move(LHS), isVolatile);
}

UQP(Expression) ParseBinary(Precedence precedence, UQP(Expression) LHS, bool isVolatile) {
	for(;;) {
		Precedence tokenPrecedence = GetTokenPrecedence();
		if (tokenPrecedence <= precedence) return LHS;

		std::string binaryOperator = CurrentOperator;
		GetNextToken();

		UQP(Expression) RHS = ParseUnary();
		if (!RHS) return nullptr;

		Precedence nextPrecedence = GetTokenPrecedence();
		if (tokenPrecedence < nextPrecedence) {
			RHS = ParseBinary((Precedence)(tokenPrecedence + 1), std::move(RHS), isVolatile);
			if (!RHS) return nullptr;
		}

		LHS = MUQ(BinaryExpression, binaryOperator, std::move(LHS), std::move(RHS), isVolatile);
	}
}

UQP(Expression) ParseUnary() {
	if (GetTokenPrecedence() == PRECEDENCE_INVALID) return ParseDispatcher();

	std::string operator_ = CurrentOperator;
	GetNextToken();
	if (UQP(Expression) operand = ParseUnary()) {
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
			// '{'
			if (CurrentToken.Value == '}') break;
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

UQP(Expression) ParseLabel() {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier name for label.");
	std::string name = CurrentIdentifier;
	GetNextToken();
	return MUQ(LabelExpression, name);
}

UQP(Expression) ParseJump() {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier name for jump.");
	std::string label = CurrentIdentifier;
	GetNextToken();
	return MUQ(JumpExpression, label);
}

UQP(Expression) ParseSizeof() {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER && CurrentToken.Subtype != LEXEME_IDENTIFIER) return ParseError("Expected type name for sizeof.");
	std::string type = CurrentIdentifier;
	GetNextToken();
	if (DefinedTypes.find(type) == DefinedTypes.end()) return ParseError("Unknown type to calculate the size of.");
	return MUQ(DwordExpression, DefinedTypes[type].Size / 8);
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

UQP(Expression) ParseWhile(bool doWhile) {
	GetNextToken();
	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis in while condition.");
	// ')'
	UQP(Expression) condition = ParseParenthetical();
	if (!condition) return nullptr;

	UQP(Expression) body = ParseExpression();
	if (!body) return nullptr;

	return MUQ(WhileExpression, std::move(condition), std::move(body), doWhile);
}

UQP(Expression) ParseDeclaration(XLSType type) {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return MUQ(VariableExpression, type.Name);
	VDX(std::string, UQP(Expression)) variableNames;

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

		if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected more identifiers after declaration.");
	}

	return MUQ(DeclarationExpression, std::move(variableNames), type);
}

UQP(Statement) ParseGlobalVariable(XLSType type) {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier after global variable declaration.", nullptr, nullptr);
	std::string name = CurrentIdentifier;
	GetNextToken();
	if (CurrentToken.Value == '=') {
		GetNextToken();
		dword value = CurrentInteger;
		GetNextToken();
		return MUQ(GlobalVariableNode, name, type, value);
	}
	return MUQ(GlobalVariableNode, name, type);
}

UQP(SignatureNode) ParseOperatorSignature() {
	if (CurrentIdentifier != "unary" && CurrentIdentifier != "binary") return ParseError("Expected operator type, unary or binary, or standard declaration/implementation of non-binary function.", nullptr);

	SignatureType signatureType = CurrentIdentifier == "unary" ? SIGNATURE_UNARY : SIGNATURE_BINARY;
	Precedence precedence = PRECEDENCE_USER_DEFAULT;

	GetNextToken();
	std::string functionName = "#op::" + CurrentIdentifier + "::#" + CurrentOperator;
	GetNextToken();

	if (CurrentToken.Type == LEXEME_INTEGER) {
		if (CurrentInteger == 0) return ParseError("Invalid precedence: must be greater than zero.", nullptr);
		precedence = (Precedence)CurrentInteger;
		GetNextToken();
	}

	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis in operator prototype.", nullptr);

	VDX(std::string, XLSType) argumentNames;
	while (GetNextToken().Type == LEXEME_IDENTIFIER)
		argumentNames.push_back(SDX(std::string, XLSType)(CurrentIdentifier, DefinedTypes["dword"]));
	if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis in operator prototype.", nullptr);
	GetNextToken();

	if (signatureType && argumentNames.size() != signatureType) return ParseError("Invalid number of operands for operator.", nullptr);
	return MUQ(SignatureNode, functionName, std::move(argumentNames), DefinedTypes["dword"], llvm::CallingConv::Fast, true, precedence);
}

UQP(SignatureNode) ParseSignature() {
	if (CurrentToken.Type != LEXEME_IDENTIFIER)
		return ParseError("Expected function name in function signature", nullptr);

	XLSType type = DefinedTypes["dword"];
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

	VDX(std::string, XLSType) argumentNames;
	GetNextToken();
	// '('
	if (CurrentToken.Value != ')') {
		for(;;) {
			XLSType currentType;
			if (CurrentToken.Subtype == LEXEME_IDENTIFIER) {
				if (DefinedTypes.find(CurrentIdentifier) == DefinedTypes.end()) return ParseError("Unknown type in parameter list of function signature.", nullptr);
				currentType = DefinedTypes[CurrentIdentifier];
				GetNextToken();
			} else currentType = DefinedTypes["dword"];

			if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier in parameter list of function signature.", nullptr);
			argumentNames.push_back(SDX(std::string, XLSType)(CurrentIdentifier, currentType));
			GetNextToken();
			if (CurrentToken.Value == ')') break;
			if (CurrentToken.Value != ',') return ParseError("Expected comma in parameter list of function signature.", nullptr);
			GetNextToken();
		}
	}
	GetNextToken();

	if (CurrentToken.Value == ':') {
		GetNextToken();
		if (CurrentToken.Subtype != LEXEME_IDENTIFIER) return ParseError("Expected type name in function signature.", nullptr);
		if (DefinedTypes.find(CurrentIdentifier) == DefinedTypes.end())
			return ParseError("Unknown type in type signature.", nullptr);
		type = DefinedTypes[CurrentIdentifier];
		GetNextToken();
	}

	return MUQ(SignatureNode, functionName, std::move(argumentNames), type, convention);
}

UQP(FunctionNode) ParseImplementation() {
	GetNextToken();
	UQP(SignatureNode) signature = ParseSignature();
	if (!signature) return nullptr;

	if (UQP(Expression) expression = ParseExpression()) {
		return MUQ(FunctionNode, std::move(signature), std::move(expression));
	}
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
		MDU(SignatureNode) signature = MUQ(SignatureNode, "__mistakeman", VDX(std::string, XLSType)(), DefinedTypes["dword"]);
		return MUQ(FunctionNode, std::move(signature), std::move(expression));
	}
	return nullptr;
}

SSA *ImplicitCast(XLSType type, SSA *toCast) {
	if (type.Type == toCast->getType()) return toCast;
	SSA *casted = Builder->CreateZExtOrTrunc(toCast, type.Type, "xls_implicit_cast");
	return casted;
}

llvm::Function *getFunction(std::string name) {
	if (llvm::Function *function = GlobalModule->getFunction(name)) return function;

	std::_Rb_tree_iterator<std::pair<const std::basic_string<char>, UQP(SignatureNode)>> signature = FunctionSignatures.find(name);
	if (signature != FunctionSignatures.end()) return signature->second->Render();

	return nullptr;
}

 Alloca *createEntryBlockAlloca(llvm::Function *function, llvm::StringRef variableName, XLSType type = DefinedTypes["dword"]) {
	llvm::IRBuilder<> apiobuilder(&function->getEntryBlock(), function->getEntryBlock().begin());
	return apiobuilder.CreateAlloca(type.Type, nullptr, variableName);
}

SSA *DwordExpression::Render() {
	return llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, Value, false));
}

SSA *VariableExpression::Render() {
	if (AllonymousValues.find(Name) == AllonymousValues.end()) {
		if (GlobalValues.find(Name) == GlobalValues.end()) CodeError("Reference to undeclared variable.");
		AnnotatedGlobal global = GlobalValues[Name];
		llvm::LoadInst *gloadInstance = Builder->CreateLoad(global.Type.Type, global.Value, Name.c_str());
		return gloadInstance;
	}
	Alloca *value = AllonymousValues[Name].Value;
	llvm::LoadInst *loadInstance = Builder->CreateLoad(AllonymousValues[Name].Type.Type, value, Name.c_str());
	loadInstance->setVolatile(Volatile);
	return loadInstance;
}

SSA *BinaryExpression::Render() {
	if (CMP("=", Operator)) {
		VariableExpression *LAssignment = static_cast<VariableExpression*>(LHS.get());
		if (!LAssignment) return CodeError("Assignment on fire.");
		SSA *value = RHS->Render();
		if (!value) return nullptr;
		SSA *variable;
		AnnotatedValue aVariable;
		if (AllonymousValues.find(LAssignment->GetName()) == AllonymousValues.end()) {
			if (GlobalValues.find(LAssignment->GetName()) == GlobalValues.end()) return CodeError("Unknown variable name.");
			AnnotatedGlobal global = GlobalValues[LAssignment->GetName()];
			variable = global.Value;
			llvm::Type *gltype = global.Type.Type;
			llvm::Type *grtype = value->getType();
			llvm::StoreInst *gstoreInstance;
			if (gltype != grtype) {
				SSA *gcasted = Builder->CreateZExtOrTrunc(value, gltype, "xls_global_cast");
				gstoreInstance = Builder->CreateStore(gcasted, variable);
			} else gstoreInstance = Builder->CreateStore(value, variable);
			gstoreInstance->setVolatile(Volatile);
			return gstoreInstance;
		}
		aVariable = AllonymousValues[LAssignment->GetName()];
		variable = aVariable.Value;
		llvm::StoreInst *storeInstance;
		// llvm::StoreInst *storeInstance = Builder->CreateStore(value, variable);
		storeInstance = Builder->CreateStore(ImplicitCast(aVariable.Type, value), variable);
		storeInstance->setVolatile(Volatile);
		return value;
	}

	if (CMP("|>", Operator)) {
		VariableExpression *RFunction = static_cast<VariableExpression*>(RHS.get());
		if (!RFunction) return CodeError("Pipe on fire.");
		SSA *lvalue = LHS->Render();
		if (!lvalue) return nullptr;
		if (DefinedTypes.find(RFunction->GetName()) != DefinedTypes.end()) {
			return ImplicitCast(DefinedTypes[RFunction->GetName()], lvalue);
		}
		llvm::Function *pipe = getFunction(RFunction->GetName());
		if (!pipe) return CodeError("Unknown function name in pipe.");
		if (pipe->arg_size() != 1) return CodeError("Can only pipe into function with one argument.");
		llvm::Argument *piped = pipe->getArg(0);
		llvm::CallInst *call = Builder->CreateCall(pipe, ImplicitCast(TypeMap[piped->getType()], lvalue), "xls_pipe");
		call->setCallingConv(pipe->getCallingConv());
		return call;
	}

#define RCAST ImplicitCast(TypeMap[left->getType()], right)
	SSA *left = LHS->Render();
	SSA *right = RHS->Render();
	if (!left || !right) return nullptr;
	JMPIF(Operator, "+", Operators_plus);
	JMPIF(Operator, "-", Operators_minus);
	JMPIF(Operator, "*", Operators_multiply);
	JMPIF(Operator, "/", Operators_divide);
	JMPIF(Operator, "<", Operators_lt_compare);
	JMPIF(Operator, ">", Operators_gt_compare);
	JMPIF(Operator, "%", Operators_modulo);
	JMPIF(Operator, "==", Operators_equal);
	JMPIF(Operator, "!=", Operators_non_equal);
	JMPIF(Operator, "&&", Operators_logical_and);
	JMPIF(Operator, "||", Operators_logical_or);
	JMPIF(Operator, "^^", Operators_logical_xor);
	JMPIF(Operator, "&", Operators_bitwise_and);
	JMPIF(Operator, "|", Operators_bitwise_or);
	JMPIF(Operator, "^", Operators_bitwise_xor);
	JMPIF(Operator, "<=", Operators_lte_compare);
	JMPIF(Operator, ">=", Operators_gte_compare);
	goto Operators_end;
 Operators_plus:
	return Builder->CreateAdd(left, right, "xls_add");
 Operators_minus:
	return Builder->CreateSub(left, right, "xls_subtract");
 Operators_multiply:
	return Builder->CreateMul(left, right, "xls_multiply");
 Operators_divide:
	return Builder->CreateUDiv(left, right, "xls_divide");
 Operators_lt_compare:
	return Builder->CreateICmpULT(left, RCAST, "xls_lt_compare");
 Operators_gt_compare:
	return Builder->CreateICmpUGT(left, RCAST, "xls_gt_compare");
 Operators_lte_compare:
	return Builder->CreateICmpULE(left, RCAST, "xls_lte_compare");
 Operators_gte_compare:
	return Builder->CreateICmpUGE(left, RCAST, "xls_gte_compare");
 Operators_equal:
	return Builder->CreateICmpEQ(left, RCAST, "xls_equal");
 Operators_non_equal:
	return Builder->CreateICmpNE(left, RCAST, "xls_non_equal");
 Operators_modulo:
	return Builder->CreateURem(left, right, "xls_modulo");
 Operators_bitwise_and:
	return Builder->CreateAnd(left, right, "xls_bitwise_and");
 Operators_bitwise_or:
	return Builder->CreateOr(left, right, "xls_bitwise_or");
 Operators_bitwise_xor:
	return Builder->CreateXor(left, right, "xls_bitwise_xor");
 Operators_logical_and:
	return Builder->CreateLogicalAnd(left, right, "xls_logical_and");
 Operators_logical_or:
	return Builder->CreateLogicalOr(left, right, "xls_logical_or");
 Operators_logical_xor:
	return Builder->CreateICmpNE(Builder->CreateLogicalOr(left, right), Builder->CreateLogicalAnd(left, right), "xls_logical_xor");
 Operators_end:
#undef RCAST

	llvm::Function *function = getFunction(std::string("#op::binary::#") + Operator);
	if (!function) return CodeError("Unknown binary operator.");
	SSA *Operands[2] = { left, right };
	llvm::CallInst* callInstance = Builder->CreateCall(function, Operands, "xls_binary_operation");
	callInstance->setCallingConv(function->getCallingConv());
	return callInstance;
}

SSA *UnaryExpression::Render() {
	SSA *operand = Operand->Render();
	if (!operand) return nullptr;

	JMPIF(Operator, "!", Unary_not);
	JMPIF(Operator, "~", Unary_ones_complement);
	goto Unary_end;
 Unary_not:
	operand = Builder->CreateZExt(operand, llvm::Type::getInt32Ty(*GlobalContext), "xls_cast_low_to_i32");
	return Builder->CreateICmpEQ(operand, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false)), "xls_unary_not");
 Unary_ones_complement:
	return Builder->CreateNot(operand, "xls_ones_complement");
 Unary_end:
	llvm::Function *function = getFunction(std::string("#op::unary::#") + Operator);
	if (!function) return CodeError("Unknown unary operator");
	llvm::CallInst *callInstance = Builder->CreateCall(function, operand, "xls_unary_operation");
	callInstance->setCallingConv(function->getCallingConv());
	return callInstance;
}

SSA *CallExpression::Render() {
	llvm::Function *called = getFunction(Called);
	if (!called) return CodeError("Call to undeclared function.");

	if (called->arg_size() != Arguments.size()) return CodeError("Argument call size mismatch with real argument size.");

	std::vector<SSA*> ArgumentVector;
	uint index = 0;

	for (llvm::Argument &argument : called->args()) {
		ArgumentVector.push_back(ImplicitCast(TypeMap[argument.getType()], Arguments[index++]->Render()));
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

SSA *LabelExpression::Render() {
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	llvm::BasicBlock *label = llvm::BasicBlock::Create(*GlobalContext, Name, function);
	Builder->CreateBr(label);
	Builder->SetInsertPoint(label);
	AllonymousLabels[Name] = label;
	return label;
}

SSA *JumpExpression::Render() {
	if (AllonymousLabels.find(Label) == AllonymousLabels.end()) return CodeError("Cannot find label.");
	return Builder->CreateBr(AllonymousLabels[Label]);
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

SSA *WhileExpression::Render() {
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	llvm::BasicBlock *loopBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_loop", function);
	llvm::BasicBlock *afterBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_after_loop", function);

	SSA *condition;
	if (!DoWhile) {
		condition = Condition->Render();
		if (!condition) return nullptr;
		if (condition->getType() != llvm::Type::getInt1Ty(*GlobalContext))
			condition = Builder->CreateICmpNE(condition, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false)), "xls_while_condition");
		Builder->CreateCondBr(condition, loopBlock, afterBlock);
	} else Builder->CreateBr(loopBlock);
	Builder->SetInsertPoint(loopBlock);
	if (!Body->Render()) return nullptr;

	condition = Condition->Render();
	if (!condition) return nullptr;
	if (condition->getType() != llvm::Type::getInt1Ty(*GlobalContext))
		condition = Builder->CreateICmpNE( condition, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false)), "xls_while_condition");

	Builder->CreateCondBr(condition, loopBlock, afterBlock);
	Builder->SetInsertPoint(afterBlock);

	return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*GlobalContext));
}

SSA *DeclarationExpression::Render() {
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	for (uint i = 0, e = VariableNames.size(); i != e; i++) {
		const std::string &variableName = VariableNames[i].first;
		Expression *definer = VariableNames[i].second.get();

		SSA *definerSSA;
		if (definer) {
			definerSSA = definer->Render();
			if (!definerSSA) return nullptr;
		} else definerSSA = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(Type.Size, 0, false));

		Alloca *alloca = createEntryBlockAlloca(function, variableName, Type);
		Builder->CreateStore(definerSSA, alloca);

		AnnotatedValue stored;
		stored.Type = Type;
		stored.Value = alloca;
		AllonymousValues[variableName] = stored;
	}

	return llvm::ConstantInt::get(*GlobalContext, llvm::APInt(Type.Size, 0, false));
}

SSA *GlobalVariableNode::Render() {
	llvm::GlobalVariable *global = new llvm::GlobalVariable(*GlobalModule, llvm::Type::getInt32Ty(*GlobalContext), false, llvm::GlobalValue::ExternalLinkage, 0, Name);
	global->setInitializer(llvm::ConstantInt::get(*GlobalContext, llvm::APInt(Type.Size, Value, false)));
	AnnotatedGlobal aGlobal;
	aGlobal.Type = Type;
	aGlobal.Value = global;
	GlobalValues[Name] = aGlobal;
	return global;
}

llvm::Function *SignatureNode::Render() {
	std::vector<llvm::Type*> ArgumentType;
	for (uint i = 0; i < Arguments.size(); i++)
		ArgumentType.push_back(Arguments[i].second.Type);

	llvm::FunctionType *functionType = llvm::FunctionType::get(Type.Type, ArgumentType, false);

	llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, Name, GlobalModule.get());
	function->setCallingConv(Convention);

	uint index = 0;
	for (llvm::Argument &argument : function->args())
		argument.setName(Arguments[index++].first);

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
		Alloca *alloca = createEntryBlockAlloca(function, argument.getName(), TypeMap[argument.getType()]);
		Builder->CreateStore(&argument, alloca);
		AnnotatedValue stored;
		stored.Type = TypeMap[argument.getType()];
		stored.Value = alloca;
		AllonymousValues[std::string(argument.getName())] = stored;
	}

  if (SSA *returnValue = Body->Render()) {
    Builder->CreateRet(ImplicitCast(signature.GetType(), returnValue));
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

	if (!Flags.NoOptimise) {
		GlobalFPM->add(llvm::createPromoteMemoryToRegisterPass());
		GlobalFPM->add(llvm::createInstructionCombiningPass());
		GlobalFPM->add(llvm::createReassociatePass());
		GlobalFPM->add(llvm::createGVNPass());
		GlobalFPM->add(llvm::createCFGSimplificationPass());
	}

	GlobalFPM->doInitialization();

	Builder = MUQ(llvm::IRBuilder<>, *GlobalContext);

	// Type definitions.
	XLSType DwordType, WordType, ByteType, BooleType;

	DwordType.Size = 32;
	DwordType.Type = llvm::Type::getInt32Ty(*GlobalContext);
	DwordType.Name = "dword";

	WordType.Size = 16;
	WordType.Type = llvm::Type::getInt16Ty(*GlobalContext);
	WordType.Name = "word";

	ByteType.Size = 8;
	ByteType.Type = llvm::Type::getInt8Ty(*GlobalContext);
	ByteType.Name = "byte";

	BooleType.Size = 1;
	BooleType.Type = llvm::Type::getInt1Ty(*GlobalContext);
	BooleType.Name = "boole";

	DefinedTypes[DwordType.Name] = DwordType;
	DefinedTypes[WordType.Name] = WordType;
	DefinedTypes[ByteType.Name] = ByteType;
	DefinedTypes[BooleType.Name] = BooleType;

	TypeMap[DwordType.Type] = DwordType;
	TypeMap[WordType.Type] = WordType;
	TypeMap[ByteType.Type] = ByteType;
	TypeMap[BooleType.Type] = BooleType;
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
			if (Flags.EmitIRToSTDOUT) functionIR->print(llvm::outs());
		}
	} else GetNextToken();
}

void HandleOperatorDefinition() {
  if (UQP(FunctionNode) functionNode = ParseOperatorDefinition()) {
    if (llvm::Function *functionIR = functionNode->Render()) {
			if (Flags.EmitIRToSTDOUT) functionIR->print(llvm::outs());
    }
  } else GetNextToken();
}

void HandleExtern() {
	if (UQP(SignatureNode) signature = ParseExtern()) {
		if (llvm::Function* signatureIR = signature->Render()) {
			if (Flags.EmitIRToSTDOUT) signatureIR->print(llvm::outs());
			FunctionSignatures[signature->GetName()] = std::move(signature);
		}
	} else GetNextToken();
}

void HandleGlobalDword() {
	if (UQP(Statement) globalDword = ParseGlobalVariable(DefinedTypes["dword"])) {
		if (SSA *globalIR = globalDword->Render()) {
			if (Flags.EmitIRToSTDOUT) globalIR->print(llvm::outs());
		}
	} else GetNextToken();
}

void HandleGlobalWord() {
	if (UQP(Statement) globalWord = ParseGlobalVariable(DefinedTypes["word"])) {
		if (SSA *globalIR = globalWord->Render()) {
			if (Flags.EmitIRToSTDOUT) globalIR->print(llvm::outs());
		}
	}
}

void HandleGlobalByte() {
  if (UQP(Statement) globalByte = ParseGlobalVariable(DefinedTypes["byte"])) {
    if (SSA *globalIR = globalByte->Render()) {
      if (Flags.EmitIRToSTDOUT)
        globalIR->print(llvm::outs());
    }
  }
}

void HandleGlobalBoole() {
	if (UQP(Statement) globalBoole = ParseGlobalVariable(DefinedTypes["boole"])) {
		if (SSA *globalIR = globalBoole->Render()) {
			if (Flags.EmitIRToSTDOUT)
				globalIR->print(llvm::outs());
		}
	}
}

void HandleUnboundedExpression() {
	if (UQP(FunctionNode) functionNode = ParseUnboundedExpression()) {
		functionNode->Render();
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
