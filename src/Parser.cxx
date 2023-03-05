#include "Parser.hxx"
#include "Lexer.hxx"
#include "colours.def.h"
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/Triple.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/SROA.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>
#include <llvm/Transforms/Vectorize.h>
#include <stack>
#include <map>
#include <stdio.h>
#include <string>

static llvm::ExitOnError ExitIfError;

llvm::DataLayout* GlobalLayout;
llvm::Triple* GlobalTriple;

ParserFlags Flags;

dword CurrentUID = 0;

Token CurrentToken;
Token GetNextToken() { return CurrentToken = GetToken(); }

std::map<std::string, Precedence> BinaryPrecedence;

UQP(llvm::LLVMContext) GlobalContext;
UQP(llvm::IRBuilder<>) Builder;
UQP(llvm::Module) GlobalModule;
UQP(llvm::legacy::FunctionPassManager) GlobalFPM;

std::map<std::string, XLSType> DefinedTypes;
std::map<llvm::Type*, XLSType> TypeMap;

std::map<std::string, UQP(SignatureNode)> FunctionSignatures;
std::map<std::string, llvm::BasicBlock*> AllonymousLabels;

std::map<llvm::Function*, XLSFunctionInfo> FunctionInfo;

std::vector<llvm::BasicBlock*> AnonymousLabels;
std::string CurrentLabelIdentifier = "current";

std::map<std::string, AnnotatedValue> AllonymousValues;
std::map<std::string, AnnotatedGlobal> GlobalValues;

std::map<SSA*, XLSType> TypeAnnotation;
std::map<llvm::Function*, XLSType> ReturnTypeAnnotation;
std::map<llvm::Argument*, XLSType> ArgumentTypeAnnotation;

std::stack<llvm::BasicBlock*> BreakStack;
std::stack<llvm::BasicBlock*> ContinueStack;

void AlertError(const char *error) {
	llvm::errs() <<
		COLOUR_YELLOW <<
		"R" << CurrentRow << "C" << CurrentColumn << ": " <<
		COLOUR_RED <<
		"Error: " << error << COLOUR_RED_BOLD <<
		COLOUR_END << "\n";
}

void AlertWarning(const char *warning) {
  llvm::errs() <<
		COLOUR_YELLOW <<
		"R" << CurrentRow << "C" << CurrentColumn << ": " <<
		COLOUR_PURPLE <<
		"Warning: " << warning << COLOUR_PURPLE_BOLD <<
		COLOUR_END << "\n";
}

#define DEFERROR(flag) { flag = 1; AlertError(error); return nullptr; }
#define DEFWARNING(flag) { flag = 1; AlertWaring(warning); return nullptr; }

UQP(Expression) ParseError(const char* error) DEFERROR(Flags.ParseError);
UQP(SignatureNode) ParseError(const char* error, void*) DEFERROR(Flags.ParseError);
UQP(Statement) ParseError(const char* error, void*, void*) DEFERROR(Flags.ParseError);
SSA *CodeError(const char* error) DEFERROR(Flags.CodeError);

UQP(Expression) ParseDwordExpression() {
	UQP(DwordExpression) result = MUQ(DwordExpression, CurrentInteger);
	GetNextToken();
	return std::move(result);
}

UQP(Expression) ParseCharacterExpression() {
	UQP(CharacterExpression) result = MUQ(CharacterExpression, CurrentToken.Value);
	GetNextToken();
	return std::move(result);
}

UQP(Expression) ParseStringExpression() {
	UQP(StringExpression) result = MUQ(StringExpression, StringLiteral, StringTerminator);
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
	if (CheckTypeDefined(identifier))
		return ParseDeclaration(DefinedTypes[CurrentIdentifier]);
	GetNextToken();
	UQP(Expression) offset;
	if (CurrentToken.Value == '[') {
		GetNextToken();
		offset = ParseExpression(isVolatile);
		if (!offset) return nullptr;
		if (CurrentToken.Value != ']') return ParseError("Expected close square parentheses in dereference offset.");
		GetNextToken();
	}
	if (CurrentToken.Value == '.') {
		GetNextToken();
		if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected field name.");
		std::string field = CurrentIdentifier;
		GetNextToken();
		if (offset) return MUQ(VariableExpression, identifier, isVolatile, false, std::move(offset), field);
		return MUQ(VariableExpression, identifier, isVolatile, false, nullptr, field);
	}
	if (offset) return MUQ(VariableExpression, identifier, isVolatile, false, std::move(offset));
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
	case LEXEME_CHARACTER_LITERAL:
		return ParseCharacterExpression();
	case LEXEME_STRING:
		return ParseStringExpression();
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
	case LEXEME_SIZEOF:
		return ParseSizeof();
	case LEXEME_TYPEOF:
		return ParseTypeof();
	case LEXEME_MUTABLE:
		return ParseMutable();
	case LEXEME_BREAK:
		return ParseBreak();
	case LEXEME_CONTINUE:
		return ParseContinue();
	case LEXEME_RETURN:
		return ParseReturn();
	case LEXEME_CVARIADIC:
		return ParseCVariadic();
	case LEXEME_VARIADIC:
		return ParseVariadic();
	case LEXEME_SLJMP:
		return ParseSetLongJump();
	case LEXEME_VOLATILE:
		GetNextToken();
		return ParseExpression(true);
	default:
		switch (CurrentToken.Value) {
		case '(': // ')'
			return ParseParenthetical();
		case '{': // '}'
			return ParseBlock();
		case ';':
			return nullptr;
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
	if (CMP("*", operator_) && CurrentToken.Type == LEXEME_IDENTIFIER) {
		std::string identifier = CurrentIdentifier;
		GetNextToken();
		return MUQ(VariableExpression, identifier, false, true);
	}

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
	if (CurrentToken.Type == LEXEME_IDENTIFIER) {
		if (CurrentIdentifier == "declare") {
			GetNextToken();
			if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected label name to forward declare.");
			std::string name = CurrentIdentifier;
			GetNextToken();
			return MUQ(LabelExpression, name, 0, LABEL_DECLARE);
		} else if (CurrentIdentifier == "define") {
			GetNextToken();
			if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected label name to define.");
		} else if (CurrentIdentifier == "set-current") {
			GetNextToken();
			if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected new name for current.");
			CurrentLabelIdentifier = CurrentIdentifier;
			GetNextToken();
			return MUQ(DwordExpression, 0);
		}
		std::string name = CurrentIdentifier;
		GetNextToken();
		return MUQ(LabelExpression, name, 0, LABEL_DEFINE);
	}
	if (CurrentToken.Type != LEXEME_INTEGER) return ParseError("Expected integer for label.");
	dword aref = CurrentInteger;
	GetNextToken();
	return MUQ(LabelExpression, "", aref, LABEL_DEFINE);
}

UQP(Expression) ParseJump() {
	GetNextToken();
	if (CurrentToken.Type == LEXEME_IDENTIFIER) {
		std::string label = CurrentIdentifier;
		GetNextToken();
		return MUQ(JumpExpression, label, 0);
	}
	if (CurrentToken.Type != LEXEME_INTEGER) return ParseError("Expected integer for jump.");
	dword aref = CurrentInteger;
	GetNextToken();
	return MUQ(JumpExpression, "", aref);
}

UQP(Expression) ParseSetLongJump() {
	bool useLongjump = CurrentToken.Subtype == LEXEME_ELSE;
	GetNextToken();
	UQP(Expression) jumpBuffer = ParseExpression();
	if (useLongjump)
		return MUQ(LongJumpExpression, std::move(jumpBuffer));
	return MUQ(SetJumpExpression, std::move(jumpBuffer));
}

UQP(Expression) ParseSizeof() {
	GetNextToken();
	if (CurrentToken.Type == LEXEME_IDENTIFIER && CheckTypeDefined(CurrentIdentifier)) {
		std::string type = CurrentIdentifier;
		GetNextToken();
		return MUQ(DwordExpression, DefinedTypes[type].Size / 8);
	}
	UQP(Expression) sized = ParseExpression();
	return MUQ(SizeofExpression, std::move(sized));
}

UQP(Expression) ParseTypeof() {
	GetNextToken();
	if (CurrentToken.Type == LEXEME_IDENTIFIER && CheckTypeDefined(CurrentIdentifier)) {
		std::string type = CurrentIdentifier;
		GetNextToken();
		return MUQ(DwordExpression, DefinedTypes[type].UID);
	}
	UQP(Expression) typed = ParseExpression();
	return MUQ(TypeofExpression, std::move(typed));
}

UQP(Expression) ParseMutable() {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_STRING) return ParseError("Invalid combination of mutable and non-string literal");
	UQP(StringExpression) result = MUQ(StringExpression, StringLiteral, StringTerminator, true);
	GetNextToken();
	return result;
}

UQP(Expression) ParseBreak() {
	GetNextToken();
	if (CurrentToken.Type == LEXEME_INTEGER) {
		dword nest = CurrentInteger;
		GetNextToken();
		return MUQ(BreakExpression, nest);
	}
	return MUQ(BreakExpression, 0);
}

UQP(Expression) ParseContinue() {
  GetNextToken();
  if (CurrentToken.Type == LEXEME_INTEGER) {
		dword nest = CurrentInteger;
		GetNextToken();
		return MUQ(ContinueExpression, nest);
	}
  return MUQ(ContinueExpression, 0);
}

UQP(Expression) ParseReturn() {
	GetNextToken();
	UQP(Expression) returnValue = ParseExpression();
	return MUQ(ReturnExpression, std::move(returnValue));
}

UQP(Expression) ParseCVariadic() {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier after C-style variadic.");
	if (!CheckTypeDefined(CurrentIdentifier)) return ParseError("Expected type after C-style variadic.");
	XLSType type = DefinedTypes[CurrentIdentifier];
	GetNextToken();
	return MUQ(CVariadicArgumentExpression, type);
}

UQP(Expression) ParseVariadic() {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier after variadic");
	if (CheckTypeDefined(CurrentIdentifier)) {
		XLSType type = DefinedTypes[CurrentIdentifier];
		GetNextToken();
		return MUQ(VariadicArgumentExpression, type);
	}
	std::string identifier = CurrentIdentifier;
	GetNextToken();
	if (CurrentToken.Value != '(') return ParseError("Expected function call.");
	GetNextToken();
	std::vector<UQP(Expression)> arguments;
	if (CurrentToken.Value != ')') {
		for (;;) {
			if (UQP(Expression) argument = ParseExpression())
				arguments.push_back(std::move(argument));
			else return nullptr;

			// '('
			if (CurrentToken.Value == ')') break;

			if (CurrentToken.Value != ',')
				return ParseError("Expected close parenthesis or comma in argument list of variadic call.");

			GetNextToken();
		}
	}
	GetNextToken();
	return MUQ(CallExpression, identifier, std::move(arguments), true);
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
	if (CurrentToken.Type == LEXEME_CHARACTER && CurrentToken.Value == '[') {
		GetNextToken();
		if (CurrentToken.Type != LEXEME_INTEGER) return ParseError("Expected integer size to statically allocate.");
		UQP(MutableArrayExpression) result = MUQ(MutableArrayExpression, CurrentInteger, type);
		GetNextToken();
		if (CurrentToken.Value != ']') return ParseError("Expected close square parenthesis in mutable array expression.");
		GetNextToken();
		return result;
	}
	if (CurrentToken.Type == LEXEME_CHARACTER && CurrentToken.Value == '(') {
		GetNextToken();
		UQP(Expression) toCast = ParseExpression();
		if (CurrentToken.Value != ')')
			return ParseError("Expected close parenthesis in cast expression.");
		GetNextToken();
		return MUQ(CastExpression, type, std::move(toCast));
	}
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return MUQ(VariableExpression, type.Name);
	if (type.Size == 0) return ParseError("Cannot declare variable of type void, or any similar type with size zero.");
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

UQP(Statement) ParseStruct() {
	GetNextToken();
	StructMode mode = STRUCT_PRACTICAL;
	if (CurrentToken.Subtype == LEXEME_STRUCT_MODE) {
		if (CMP("packed", CurrentIdentifier)) mode = STRUCT_PACKED;
		else if (CMP("practical", CurrentIdentifier)) mode = STRUCT_PRACTICAL;
		else if (CMP("padded", CurrentIdentifier)) mode = STRUCT_PADDED;
		GetNextToken();
	}
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected structure name.", nullptr, nullptr);
	std::string name = CurrentIdentifier;
	GetNextToken();
	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis in structure definition.", nullptr, nullptr);
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER || !CheckTypeDefined(CurrentIdentifier)) return ParseError("Expected at least one field in struct.", nullptr, nullptr);
	VDX(XLSType, std::string) types;
	XLSType currentType = DefinedTypes[CurrentIdentifier];
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected at least one named field in struct.", nullptr, nullptr);
	types.push_back(SDX(XLSType, std::string)(currentType, CurrentIdentifier));
	GetNextToken();
	if (CurrentToken.Value == ',') {
		GetNextToken();
		for(;;) {
			if (CurrentToken.Type != LEXEME_IDENTIFIER || !CheckTypeDefined(CurrentIdentifier)) return ParseError("Expected field in struct.", nullptr, nullptr);
			currentType = DefinedTypes[CurrentIdentifier];
			GetNextToken();
			if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected named field in struct.", nullptr, nullptr);
			types.push_back(SDX(XLSType, std::string)(currentType, CurrentIdentifier));
			GetNextToken();
			if (CurrentToken.Value == ')') break;
			if (CurrentToken.Value != ',') return ParseError("Expected comma in struct definition's field list.", nullptr, nullptr);
			GetNextToken();
		}
	}
	GetNextToken();
	return MUQ(StructDefinition, types, name, mode);
}

UQP(Statement) ParseTypedef() {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier after typedef.", nullptr, nullptr);
	if (CMP(CurrentIdentifier, "alias")) {
		GetNextToken();
		if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected typename to alias.", nullptr, nullptr);
		if (CheckTypeDefined(CurrentIdentifier)) return ParseError("Type already exists, cannot be aliased.", nullptr, nullptr);
		std::string destinationName = CurrentIdentifier;
		XLSType destination;
		GetNextToken();
		if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier to alias type to.", nullptr, nullptr);
		if (!CheckTypeDefined(CurrentIdentifier)) return ParseError("Expected type to act as source for alias.", nullptr, nullptr);
		XLSType source = DefinedTypes[CurrentIdentifier];
		destination = source;
		destination.Name = destinationName;
		DefinedTypes[destination.Name] = destination;
		GetNextToken();
		return MUQ(NullNode);
	}
	return ParseError("Unknown typedef operand.", nullptr, nullptr);
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
	return MUQ(SignatureNode, functionName, std::move(argumentNames), DefinedTypes["dword"], VARIADIC_NONE, llvm::CallingConv::Fast, true, precedence);
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
	Variadism variadic;
	GetNextToken();
	// '('
	if (CurrentToken.Value != ')') {
		for(;;) {
			XLSType currentType;
			if (CheckTypeDefined(CurrentIdentifier)) {
				currentType = DefinedTypes[CurrentIdentifier];
				GetNextToken();
			} else currentType = DefinedTypes["dword"];

			if (CurrentToken.Subtype == LEXEME_VARIADIC) {
				variadic = CurrentToken.Type == LEXEME_VARIADIC ? VARIADIC_XLS : VARIADIC_C;
				GetNextToken();
				// '('
				if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis after variadic.", nullptr);
				break;
			}
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
		if (!CheckTypeDefined(CurrentIdentifier))
			return ParseError("Unknown type in type signature.", nullptr);
		type = DefinedTypes[CurrentIdentifier];
		GetNextToken();
	}

	return MUQ(SignatureNode, functionName, std::move(argumentNames), type, variadic, convention);
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
		UQP(SignatureNode) signature = MUQ(SignatureNode, "__mistakeman", VDX(std::string, XLSType)(), DefinedTypes["dword"]);
		return MUQ(FunctionNode, std::move(signature), std::move(expression));
	}
	return nullptr;
}

XLSType GetType(SSA *toType) {
	if (TypeAnnotation.find(toType) == TypeAnnotation.end())
		TypeAnnotation[toType] = TypeMap[toType->getType()];
	return TypeAnnotation[toType];
}

bool operator== (XLSType A, XLSType B) {
	return A.Name == B.Name;
}

bool operator< (XLSType A, XLSType B) {
	return A.Size < B.Size;
}

bool operator<= (XLSType A, XLSType B) {
	return A.Size <= B.Size;
}

bool operator< (SDX(XLSType, std::string) A, SDX(XLSType, std::string) B) {
	return A.first.Size < B.first.Size;
}

bool operator<= (SDX(XLSType, std::string) A, SDX(XLSType, std::string) B) {
  return A.first.Size <= B.first.Size;
}

SSA *ImplicitCast(XLSType type, SSA *toCast) {
	if (type == GetType(toCast)) return toCast;
	SSA *casted;
	TypeAnnotation[casted] = type;
	if (!type.UID || !GetType(toCast).UID) return casted = llvm::PoisonValue::get(type.Type);
	if (type.Type->isVoidTy() || toCast->getType()->isVoidTy())
		return casted = llvm::Constant::getNullValue(type.Type);
	if (type.IsPointer && toCast->getType()->isPointerTy())
		return casted = Builder->CreateBitCast(toCast, type.Type, "xls_ptp_cast");
	if (type.IsPointer) return casted = Builder->CreateIntToPtr(toCast, type.Type, "xls_itp_cast");
	if (toCast->getType()->isPointerTy())
		return casted = Builder->CreatePtrToInt(toCast, type.Type, "xls_pti_cast");
	if (type.IsStruct) return casted = Builder->CreateBitCast(toCast, type.Type, "xls_bitcast");
	if (type.Signed) return casted = Builder->CreateSExtOrTrunc(toCast, type.Type, "xls_simplicit_cast");
	casted = Builder->CreateZExtOrTrunc(toCast, type.Type, "xls_implicit_cast");
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
  SSA *R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, Value, false));
	TypeAnnotation[R] = DefinedTypes["dword"];
  return R;
}

SSA *CharacterExpression::Render() {
  SSA *R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(8, Value, false));
	TypeAnnotation[R] = DefinedTypes["byte"];
  return R;
}

SSA *StringExpression::Render() {
	std::vector<llvm::Constant*> elements(Value.size());
	for (unsigned i = 0; i < Value.size(); i++) {
		elements[i] = llvm::ConstantInt::get(DefinedTypes["byte"].Type, Value[i]);
	}

	if (Terminator == ST_NULL) elements.push_back(llvm::ConstantInt::get(DefinedTypes["byte"].Type, 0x00));

	llvm::ArrayType* strType = llvm::ArrayType::get(DefinedTypes["byte"].Type, elements.size());

	llvm::GlobalVariable* global = new llvm::GlobalVariable(*GlobalModule, strType, !Mutable, llvm::GlobalValue::PrivateLinkage, llvm::ConstantArray::get(strType, elements), "xls_string");
	SSA *R = llvm::ConstantExpr::getBitCast(global, DefinedTypes["byte*"].Type);
	TypeAnnotation[R] = DefinedTypes["byte*"];
	return R;
}

SSA *MutableArrayExpression::Render() {
	if (!CheckTypeDefined(Type.Name + "*")) return nullptr;
	llvm::ArrayType* maType = llvm::ArrayType::get(Type.Type, Size);
	llvm::GlobalVariable* global = new llvm::GlobalVariable(*GlobalModule, maType, false, llvm::GlobalValue::PrivateLinkage, llvm::Constant::getNullValue(maType), "xls_mutable_array");
	global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
	SSA *R = llvm::ConstantExpr::getBitCast(global, DefinedTypes[Type.Name + "*"].Type);
	TypeAnnotation[R] = DefinedTypes[Type.Name + "*"];
	return R;
}

SSA *VariableExpression::Render() {
	if (AllonymousValues.find(Name) == AllonymousValues.end()) {
		if (FunctionSignatures.find(Name) != FunctionSignatures.end()) {
			llvm::Function *pointed = getFunction(Name);
			XLSType FPType;
			if (!DefineFPType(Name, &FPType)) return nullptr;
			TypeAnnotation[pointed] = FPType;
			return pointed;
		}
		if (GlobalValues.find(Name) == GlobalValues.end()) CodeError("Reference to undeclared variable.");
		AnnotatedGlobal global = GlobalValues[Name];
		llvm::LoadInst *gloadInstance = Builder->CreateLoad(global.Type.Type, global.Value, Name.c_str());
		TypeAnnotation[gloadInstance] = global.Type;
		return gloadInstance;
	}
	llvm::LoadInst *loadInstance;

	AnnotatedValue A = AllonymousValues[Name];
	if (Dereference) {
		UQP(Expression) V = MUQ(VariableExpression, Name, Volatile, false);
		UQP(UnaryExpression) U = MUQ(UnaryExpression, "*", std::move(V));
		SSA *R = U->Render();
		return R;
	}

	if (Field != "") {
		if (!A.Type.IsStruct) return nullptr;
		if (A.Type.Structure.Fields.find(Field) == A.Type.Structure.Fields.end()) return CodeError("Unknown field.");
		dword fieldOffset = A.Type.Structure.Fields[Field].first;
		XLSType fieldType = A.Type.Structure.Fields[Field].second;
		std::vector<SSA*> GEPIndex(2);
		GEPIndex[0] = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false));
		GEPIndex[1] = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, fieldOffset, false));
		SSA *GEP = Builder->CreateGEP(A.Type.Type, A.Value, GEPIndex, "xls_gep");
		loadInstance = Builder->CreateLoad(fieldType.Type, GEP, Name.c_str());
		TypeAnnotation[loadInstance] = fieldType;
		return loadInstance;
	}

	if (Offset) {
		SSA *offset = Offset->Render();
		if (!offset) return nullptr;
		SSA *V = Builder->CreateLoad(A.Type.Type, A.Value, Name.c_str());
		SSA *GEP = Builder->CreateInBoundsGEP(DefinedTypes[A.Type.Dereference].Type, V, offset);
		loadInstance = Builder->CreateLoad(DefinedTypes[A.Type.Dereference].Type, GEP);
		loadInstance->setVolatile(Volatile);
		TypeAnnotation[loadInstance] = DefinedTypes[A.Type.Dereference];
		return loadInstance;
	}

	loadInstance = Builder->CreateLoad(A.Type.Type, A.Value, Name.c_str());
	loadInstance->setVolatile(Volatile);
	TypeAnnotation[loadInstance] = A.Type;
	return loadInstance;
}

SSA *CastExpression::Render() {
	SSA *toCast = Value->Render();
	if (!toCast) return nullptr;
	SSA *R = ImplicitCast(Type, toCast);
	TypeAnnotation[R] = Type;
	return R;
}

SSA *PtrRHS(SSA *left, SSA *right) {
  return ImplicitCast(GetType(left), Builder->CreateMul(right, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(GetType(right).Size, DefinedTypes[GetType(left).Dereference].Size / 8))));
}

SSA *BinaryExpression::Render() {
	if (CMP("=", Operator)) {
		VariableExpression *LAssignment = static_cast<VariableExpression*>(LHS.get());
		if (!LAssignment) return CodeError("Assignment on fire.");
		SSA *value = RHS->Render();
		if (!value) return nullptr;
		if (!GetType(value).UID) value = llvm::PoisonValue::get(value->getType());
		SSA *variable;
		llvm::StoreInst *storeInstance;
		AnnotatedValue A;
		if (AllonymousValues.find(LAssignment->GetName()) == AllonymousValues.end()) {
			if (GlobalValues.find(LAssignment->GetName()) == GlobalValues.end()) return CodeError("Unknown variable name.");
			AnnotatedGlobal global = GlobalValues[LAssignment->GetName()];
			SSA *castedValue = ImplicitCast(global.Type, value);
			storeInstance = Builder->CreateStore(castedValue, global.Value);
			storeInstance->setVolatile(Volatile);
			TypeAnnotation[castedValue] = global.Type;
			return castedValue;
		}
		A = AllonymousValues[LAssignment->GetName()];
		if (LAssignment->GetField() != "") {
			if (!A.Type.IsStruct) return nullptr;
			if (A.Type.Structure.Fields.find(LAssignment->GetField()) == A.Type.Structure.Fields.end()) return CodeError("Unknown field to assign.");
			dword fieldOffset = A.Type.Structure.Fields[LAssignment->GetField()].first;
			XLSType fieldType = A.Type.Structure.Fields[LAssignment->GetField()].second;
			std::vector<SSA*> GEPIndex(2);
			GEPIndex[0] = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false));
			GEPIndex[1] = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, fieldOffset, false));
			SSA *GEP = Builder->CreateGEP(A.Type.Type, A.Value, GEPIndex, "xls_assign_gep");
			SSA *castedValue = ImplicitCast(fieldType, value);
			storeInstance = Builder->CreateStore(castedValue, GEP);
			storeInstance->setVolatile(Volatile);
			TypeAnnotation[castedValue] = fieldType;
			return castedValue;
		}
		if (LAssignment->GetOffset()) {
			SSA *offset = LAssignment->GetOffset()->Render();
			SSA *castedValue = ImplicitCast(DefinedTypes[A.Type.Dereference], value);
			SSA *V = Builder->CreateLoad(A.Type.Type, A.Value, LAssignment->GetName());
			SSA *GEP = Builder->CreateInBoundsGEP(DefinedTypes[A.Type.Dereference].Type, V, offset);
			storeInstance = Builder->CreateStore(castedValue, GEP);
			storeInstance->setVolatile(Volatile);
			TypeAnnotation[castedValue] = DefinedTypes[A.Type.Dereference];
			return castedValue;
		}
		if (LAssignment->IsDereference()) {
			if (!A.Type.IsPointer) return CodeError("Non-pointer values cannot be dereferenced.");
			storeInstance = Builder->CreateStore(ImplicitCast(DefinedTypes[A.Type.Dereference], value), Builder->CreateLoad(A.Type.Type, A.Value, "xls_assign_pointer"));
			storeInstance->setVolatile(Volatile);
			TypeAnnotation[value] = DefinedTypes[A.Type.Dereference];
			return ImplicitCast(DefinedTypes[A.Type.Dereference], value);
		}
		variable = A.Value;
		storeInstance = Builder->CreateStore(ImplicitCast(A.Type, value), variable);
		storeInstance->setVolatile(Volatile);
		TypeAnnotation[storeInstance] = A.Type;
		return value;
	}

	if (CMP("|>", Operator)) {
		VariableExpression *RFunction = static_cast<VariableExpression*>(RHS.get());
		if (!RFunction) return CodeError("Pipe on fire.");
		SSA *lvalue = LHS->Render();
		if (!lvalue) return nullptr;
		if (CheckTypeDefined(RFunction->GetName())) {
			return ImplicitCast(DefinedTypes[RFunction->GetName()], lvalue);
		}
		llvm::Function *pipe = getFunction(RFunction->GetName());
		if (!pipe) return CodeError("Unknown function name in pipe.");
		if (pipe->arg_size() != 1) return CodeError("Can only pipe into function with one argument.");
		llvm::Argument *piped = pipe->getArg(0);
		llvm::CallInst *call = Builder->CreateCall(pipe, ImplicitCast(GetType(piped), lvalue), "xls_pipe");
		call->setCallingConv(pipe->getCallingConv());
		call->setAttributes(pipe->getAttributes());
		return call;
	}

#define RCAST ImplicitCast(GetType(left), right)
#define RPTR left->getType()->isPointerTy() ? PtrRHS(left, right) : right
#define RPCAST left->getType()->isPointerTy() ? PtrRHS(left, RCAST) : RCAST
	SSA *R;
#define RET(V) do { R = V; TypeAnnotation[R] = GetType(left); return R; } while(0)
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
	RET(Builder->CreateAdd(left, RPCAST, "xls_add"));
 Operators_minus:
	RET(Builder->CreateSub(left, RPCAST, "xls_subtract"));
 Operators_multiply:
	RET(Builder->CreateMul(left, RPCAST, "xls_multiply"));
 Operators_divide:
	RET(Builder->CreateUDiv(left, RPCAST, "xls_divide"));
 Operators_lt_compare:
	if (GetType(left).Signed || GetType(right).Signed)
		RET(Builder->CreateICmpSLT(left, RCAST, "xls_slt_compare"));
	RET(Builder->CreateICmpULT(left, RCAST, "xls_lt_compare"));
 Operators_gt_compare:
	if (GetType(left).Signed || GetType(right).Signed)
		RET(Builder->CreateICmpSGT(left, RCAST, "xls_sgt_compare"));
	RET(Builder->CreateICmpUGT(left, RCAST, "xls_gt_compare"));
 Operators_lte_compare:
	if (GetType(left).Signed || GetType(right).Signed)
		RET(Builder->CreateICmpSLE(left, RCAST, "xls_slte_compare"));
	RET(Builder->CreateICmpULE(left, RCAST, "xls_lte_compare"));
 Operators_gte_compare:
	if (GetType(left).Signed || GetType(right).Signed)
		RET(Builder->CreateICmpSGE(left, RCAST, "xls_sgte_compare"));
	RET(Builder->CreateICmpUGE(left, RCAST, "xls_gte_compare"));
 Operators_equal:
	RET(Builder->CreateICmpEQ(left, RCAST, "xls_equal"));
 Operators_non_equal:
	RET(Builder->CreateICmpNE(left, RCAST, "xls_non_equal"));
 Operators_modulo:
	RET(Builder->CreateURem(left, RPCAST, "xls_modulo"));
 Operators_bitwise_and:
	RET(Builder->CreateAnd(left, right, "xls_bitwise_and"));
 Operators_bitwise_or:
	RET(Builder->CreateOr(left, right, "xls_bitwise_or"));
 Operators_bitwise_xor:
	RET(Builder->CreateXor(left, right, "xls_bitwise_xor"));
 Operators_logical_and:
	RET(Builder->CreateLogicalAnd(left, right, "xls_logical_and"));
 Operators_logical_or:
	RET(Builder->CreateLogicalOr(left, right, "xls_logical_or"));
 Operators_logical_xor:
	RET(Builder->CreateICmpNE(Builder->CreateLogicalOr(left, right), Builder->CreateLogicalAnd(left, right), "xls_logical_xor"));
 Operators_end:
#undef RPCAST
#undef RCAST
#undef RPTR
#undef RET

	llvm::Function *function = getFunction(std::string("#op::binary::#") + Operator);
	if (!function) return CodeError("Unknown binary operator.");
	SSA *Operands[2] = { left, right };
	llvm::CallInst* callInstance = Builder->CreateCall(function, Operands, "xls_binary_operation");
	callInstance->setCallingConv(function->getCallingConv());
	TypeAnnotation[callInstance] = GetType(callInstance);
	return callInstance;
 }

SSA *UnaryExpression::Render() {
	if(CMP("&", Operator)) {
		VariableExpression *LAssignment = static_cast<VariableExpression*>(Operand.get());
		if (!LAssignment) return CodeError("Address-of operation on fire.");
		AnnotatedValue V;
		if (AllonymousValues.find(LAssignment->GetName()) == AllonymousValues.end())
			return CodeError("Unknown variable name to address.");
		V = AllonymousValues[LAssignment->GetName()];
		SSA *value;
		XLSType PtrType;
		PtrType.Dereference = V.Type.Name;
		PtrType.Name = V.Type.Name;
		PtrType.Name.push_back('*');
		PtrType.IsPointer = true;
		PtrType.Type = V.Type.Type->getPointerTo();
		PtrType.Size = GlobalLayout->getPointerSizeInBits();
		if (!CheckTypeDefined(PtrType.Name)) return nullptr;
		TypeAnnotation[value] = PtrType;
		value = V.Value;
		return value;
	}

	if(CMP("&&", Operator)) {
		VariableExpression *LAssignment = static_cast<VariableExpression*>(Operand.get());
		if (!LAssignment) return CodeError("Label address-of operation on fire.");
		llvm::Function* function = Builder->GetInsertBlock()->getParent();
		if (LAssignment->GetName() == CurrentLabelIdentifier) {
			llvm::BasicBlock* Label = AnonymousLabels.back();
			SSA *Address = llvm::BlockAddress::get(function, Label);
			TypeAnnotation[Address] = DefinedTypes["label&"];
			return Address;
		}
		if (AllonymousLabels.find(LAssignment->GetName()) == AllonymousLabels.end()) AllonymousLabels[LAssignment->GetName()] = llvm::BasicBlock::Create(*GlobalContext, "L#" + LAssignment->GetName(), function);
		llvm::BasicBlock* Label = AllonymousLabels[LAssignment->GetName()];
		SSA *Address = llvm::BlockAddress::get(function, Label);
		TypeAnnotation[Address] = DefinedTypes["label&"];
		return Address;
	}

	SSA *operand = Operand->Render();
	if (!operand) return nullptr;

	SSA *R;
#define RET(V) R = V; TypeAnnotation[R] = GetType(operand); return R
	JMPIF(Operator, "!", Unary_not);
	JMPIF(Operator, "~", Unary_ones_complement);
	JMPIF(Operator, "*", Unary_dereference);
	goto Unary_end;
 Unary_not:
	operand = Builder->CreateZExt(operand, llvm::Type::getInt32Ty(*GlobalContext), "xls_cast_low_to_i32");
	RET(Builder->CreateICmpEQ(operand, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false)), "xls_unary_not"));
 Unary_ones_complement:
	RET(Builder->CreateNot(operand, "xls_ones_complement"));
 Unary_dereference:
	RET(Builder->CreateLoad(DefinedTypes[TypeMap[operand->getType()].Dereference].Type, operand, "xls_dereference"));
 Unary_end:
	llvm::Function *function = getFunction(std::string("#op::unary::#") + Operator);
	if (!function) return CodeError("Unknown unary operator");
	llvm::CallInst *callInstance = Builder->CreateCall(function, operand, "xls_unary_operation");
#undef RET
	callInstance->setCallingConv(function->getCallingConv());
	TypeAnnotation[callInstance] = GetType(callInstance);
	return callInstance;
}

SSA *CallExpression::Render() {
	llvm::Function *called = getFunction(Called);
	if (!called) {
		if (AllonymousValues.find(Called) == AllonymousValues.end())
			return CodeError("Call to undeclared function.");

		AnnotatedValue A = AllonymousValues[Called];
		if (!A.Type.IsFP) return CodeError("Call to non-function pointer.");
		llvm::LoadInst* FP = Builder->CreateLoad(A.Type.Type, A.Value, "xls_fp_load");

		if (A.Type.FPData.size() != Arguments.size() + 1) return CodeError("Argument call size mismatch with type argument size.");

		std::vector<SSA*> FPArguments;
		for (uint i = 0; i < Arguments.size(); i++)
			FPArguments.push_back(ImplicitCast(A.Type.FPData[i + 1], Arguments[i]->Render()));

		std::vector<llvm::Type*> FPParams;
		for (uint i = 1; i < A.Type.FPData.size(); i++)
			FPParams.push_back(A.Type.FPData[i].Type);

		llvm::FunctionType* FPType = llvm::FunctionType::get(A.Type.FPData[0].Type, llvm::ArrayRef<llvm::Type*>(FPParams), false);
		llvm::CallInst* callInstance = Builder->CreateCall(FPType, FP, FPArguments, "xls_fp_call");
		if (A.Type.FPData[0].UID == DefinedTypes["void"].UID) callInstance->setName("");
		TypeAnnotation[callInstance] = A.Type.FPData[0];
		return callInstance;
	}

	if (called->arg_size() != Arguments.size() && FunctionInfo[called].Variadic == VARIADIC_NONE) return CodeError("Argument call size mismatch with real argument size.");

	std::vector<SSA*> ArgumentVector;
	uint index = 0;

	for (llvm::Argument &argument : called->args()) {
		if (!TailVariadic && FunctionInfo[called].Variadic == VARIADIC_XLS && index == called->arg_size() - 1) {
			std::vector<SSA*> HiveVector;
			uint HiveBitLength = 0;
			for(; index < Arguments.size(); index++) {
				SSA* bee = Arguments[index]->Render();
				if (!bee) return nullptr;
				HiveVector.push_back(bee);
				HiveBitLength += GetType(bee).Size;
			}
			llvm::Type* HiveType = llvm::Type::getIntNTy(*GlobalContext, HiveBitLength);
			llvm::Function* function = Builder->GetInsertBlock()->getParent();
			llvm::IRBuilder<> apiobuilder(&function->getEntryBlock(), function->getEntryBlock().begin());
			SSA* Hive = apiobuilder.CreateAlloca(HiveType, nullptr, "xls_hive");
			SSA *HivePTR = Builder->CreateBitCast(Hive, DefinedTypes["byte*"].Type);
			for (uint i = 0, offset = 0; i < HiveVector.size(); i++) {
				SSA* SOFFSET = llvm::ConstantInt::get(DefinedTypes["dword"].Type, llvm::APInt(32, offset, false));
				SSA* GEP = Builder->CreateInBoundsGEP(DefinedTypes["byte"].Type, HivePTR, SOFFSET);
				Builder->CreateStore(HiveVector[i], Builder->CreateBitCast(GEP, HiveVector[i]->getType()->getPointerTo()));
				offset += GetType(HiveVector[i]).Size / 8;
			}
			ArgumentVector.push_back(HivePTR);
			break;
		} else if (TailVariadic && index == called->arg_size() - 1) {
			if (FunctionInfo[called].Variadic != VARIADIC_XLS) return CodeError("Cannot pass the variadic hive to non-variadic function.");
			AnnotatedValue HiveStore = AllonymousValues["#hive"];
			SSA* Hive = Builder->CreateLoad(HiveStore.Type.Type, HiveStore.Value, "xls_pass_hive");
			ArgumentVector.push_back(Hive);
			break;
		}
		ArgumentVector.push_back(ImplicitCast(ArgumentTypeAnnotation[called->getArg(index)], Arguments[index]->Render()));
		index++;
		if (!ArgumentVector.back()) return nullptr;
	}

	if (called->isVarArg()) {
		for (; index < Arguments.size(); index++) {
			ArgumentVector.push_back(Arguments[index]->Render());
		}
	}

	llvm::CallInst* callInstance = Builder->CreateCall(called, ArgumentVector, "xls_call");
	if (called->getReturnType() == llvm::Type::getVoidTy(*GlobalContext))
		callInstance->setName("");
	callInstance->setCallingConv(called->getCallingConv());
	callInstance->setAttributes(called->getAttributes());
	TypeAnnotation[callInstance] = ReturnTypeAnnotation[called];
	return callInstance;
}

SSA *BreakExpression::Render() {
	llvm::BasicBlock* location;
	std::stack<llvm::BasicBlock*> preserve;
	for (uint i = 0; i <= Nest; i++) {
		if (BreakStack.empty()) return CodeError("Break stack is empty.");
		location = BreakStack.top();
		preserve.push(BreakStack.top());
		BreakStack.pop();
	}
	Builder->CreateBr(location);
	RESOW_BASIC_BLOCK;
	for (uint i = 0; i < preserve.size(); i++) {
		BreakStack.push(preserve.top());
		preserve.pop();
	}
	SSA *R = llvm::Constant::getNullValue(DefinedTypes["dword"].Type);
	TypeAnnotation[R] = DefinedTypes["dword"];
	return R;
}

SSA *ContinueExpression::Render() {
  llvm::BasicBlock *location;
	std::stack<llvm::BasicBlock*> preserve;
  for (uint i = 0; i <= Nest; i++) {
    if (ContinueStack.empty()) return CodeError("Continue stack is empty.");
    location = ContinueStack.top();
		preserve.push(ContinueStack.top());
    ContinueStack.pop();
  }
  Builder->CreateBr(location);
	RESOW_BASIC_BLOCK;
	for (uint i = 0; i < preserve.size(); i++) {
		ContinueStack.push(preserve.top());
		preserve.pop();
	}
  SSA *R = llvm::Constant::getNullValue(DefinedTypes["dword"].Type);
  TypeAnnotation[R] = DefinedTypes["dword"];
  return R;
}

SSA *BlockExpression::Render() {
	std::vector<SSA*> ExpressionVector;
	for (uint i = 0, e = Expressions.size(); i != e; i++) {
		ExpressionVector.push_back(Expressions[i]->Render());
		if (!ExpressionVector.back()) return nullptr;
	}

	return ExpressionVector.back();
}

SSA *ReturnExpression::Render() {
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	SSA *R = llvm::Constant::getNullValue(DefinedTypes["dword"].Type);
	TypeAnnotation[R] = DefinedTypes["dword"];
	SSA *returnValue;
	if (!ReturnValue) returnValue = llvm::Constant::getNullValue(function->getReturnType());
	else returnValue = ImplicitCast(ReturnTypeAnnotation[function], ReturnValue->Render());
	if (!returnValue) return nullptr;

	if (ReturnTypeAnnotation[function].UID == DefinedTypes["void"].UID) Builder->CreateRetVoid();
	else Builder->CreateRet(returnValue);
	RESOW_BASIC_BLOCK;
	return R;
}

SSA *CVariadicArgumentExpression::Render() {
	AnnotatedValue VAListStore = AllonymousValues["variadic"];
	SSA* VAList = Builder->CreateBitCast(VAListStore.Value, llvm::Type::getInt8PtrTy(*GlobalContext));
	SSA* R = Builder->CreateVAArg(VAList, Type.Type, "xls_variadic_get");
	TypeAnnotation[R] = Type;
	return R;
}

SSA *VariadicArgumentExpression::Render() {
	AnnotatedValue HiveStore = AllonymousValues["#hive"];
	SSA* Hive = Builder->CreateLoad(HiveStore.Type.Type, HiveStore.Value, "xls_hive_value");
	SSA* bee = Builder->CreateBitCast(Hive, Type.Type->getPointerTo());
	SSA* R = Builder->CreateLoad(Type.Type, bee);
	SSA* GEP = Builder->CreateInBoundsGEP(Type.Type, bee, llvm::ConstantInt::get(DefinedTypes["dword"].Type, llvm::APInt(32, 1, false)));
	Builder->CreateStore(Builder->CreateBitCast(GEP, DefinedTypes["byte*"].Type), HiveStore.Value);
	TypeAnnotation[R] = Type;
	return R;
}

SSA *LabelExpression::Render() {
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	SSA *R = llvm::Constant::getNullValue(DefinedTypes["dword"].Type);
	TypeAnnotation[R] = DefinedTypes["dword"];
	if (Name == CurrentLabelIdentifier && Mode == LABEL_DECLARE) {
		llvm::BasicBlock *NEW = llvm::BasicBlock::Create(*GlobalContext, "AL" + std::to_string(AnonymousLabels.size()), function);
		AnonymousLabels.push_back(NEW);
		return R;
	} else if (Name == CurrentLabelIdentifier && Mode == LABEL_DEFINE) {
		llvm::BasicBlock* label = AnonymousLabels.back();
		Builder->CreateBr(label);
		Builder->SetInsertPoint(label);
		return R;
	} else if (Name == "") {
		if (AnonymousReferer > AnonymousLabels.size()) {
			for(uint i = AnonymousLabels.size(); i <= AnonymousReferer; i++)
				AnonymousLabels.push_back(llvm::BasicBlock::Create(*GlobalContext, "AL" + std::to_string(i), function));
		}
		llvm::BasicBlock* label = AnonymousLabels[AnonymousReferer];
		Builder->CreateBr(label);
		Builder->SetInsertPoint(label);
		return R;
	}
	if (AllonymousLabels.find(Name) == AllonymousLabels.end()) AllonymousLabels[Name] = llvm::BasicBlock::Create(*GlobalContext, "L#" + Name, function);
	if (Mode == LABEL_DEFINE) {
		llvm::BasicBlock* label = AllonymousLabels[Name];
		Builder->CreateBr(label);
		Builder->SetInsertPoint(label);
	}
	return R;
}

SSA *JumpExpression::Render() {
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	SSA *R = llvm::Constant::getNullValue(DefinedTypes["dword"].Type);
	TypeAnnotation[R] = DefinedTypes["dword"];
	if (Label == CurrentLabelIdentifier) {
		llvm::BasicBlock* label = AnonymousLabels.back();
		Builder->CreateBr(label);
		RESOW_BASIC_BLOCK;
		return R;
	} else if (Label == "") {
		if (AnonymousReferer > AnonymousLabels.size()) {
			for (uint i = AnonymousLabels.size(); i <= AnonymousReferer; i++)
				AnonymousLabels.push_back(llvm::BasicBlock::Create(*GlobalContext, "AL" + std::to_string(i), function));
		}
		llvm::BasicBlock *label = AnonymousLabels[AnonymousReferer];
		Builder->CreateBr(label);
		RESOW_BASIC_BLOCK;
		return R;
	}
	// TODO: Jump to any expression that returns a label pointer.
	if (AllonymousValues.find(Label) != AllonymousValues.end()) {
		AnnotatedValue A = AllonymousValues[Label];
		llvm::LoadInst* V = Builder->CreateLoad(A.Type.Type, A.Value, "xls_jump&");
		llvm::IndirectBrInst* B = Builder->CreateIndirectBr(V, AllonymousLabels.size() + AnonymousLabels.size());
		for (std::pair<std::string, llvm::BasicBlock *> dest : AllonymousLabels) {
			B->addDestination(dest.second);
		}
		for (uint i = 0; i < AnonymousLabels.size(); i++) B->addDestination(AnonymousLabels[i]);
		RESOW_BASIC_BLOCK;
		return R;
	}
	if (AllonymousLabels.find(Label) == AllonymousLabels.end()) AllonymousLabels[Label] = llvm::BasicBlock::Create(*GlobalContext, "L#" + Label, function);
	llvm::BasicBlock* label = AllonymousLabels[Label];
	Builder->CreateBr(label);
	RESOW_BASIC_BLOCK;
	return R;
}

SSA* SetJumpExpression::Render() {
  llvm::Function* SetJumpIntrinsic = llvm::Intrinsic::getDeclaration(GlobalModule.get(), llvm::Intrinsic::eh_sjlj_setjmp);
	SetJumpIntrinsic->addFnAttr(llvm::Attribute::AlwaysInline);
	SetJumpIntrinsic->addFnAttr(llvm::Attribute::ReturnsTwice);
	llvm::Function* FrameAddrIntrinsic = llvm::Intrinsic::getDeclaration(GlobalModule.get(), llvm::Intrinsic::returnaddress);
	FrameAddrIntrinsic->setName("llvm.frameaddress.p0"); // Necessary to prevent bug in LLVM where this specific intrinsic segmentationally faults when one tries to get its declaration.
	FrameAddrIntrinsic->addFnAttr(llvm::Attribute::AlwaysInline);
	llvm::Function* StackSaveIntrinsic = llvm::Intrinsic::getDeclaration(GlobalModule.get(), llvm::Intrinsic::stacksave); // Undocumented -_-: must also store the stack pointer in the third 'word' of the jump buffer
	SSA* jumpBuffer = JumpBuffer->Render();
	std::vector<llvm::Type*> IType;
	std::vector<SSA*> IArg;
	IType.push_back(DefinedTypes["dword"].Type);
	IArg.push_back(llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false)));
	SSA* frameAddr = Builder->CreateCall(FrameAddrIntrinsic, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false)));
	TypeAnnotation[frameAddr] = DefinedTypes["byte*"];
	Builder->CreateStore(frameAddr, Builder->CreateBitCast(jumpBuffer, DefinedTypes["#addrsize"].Type->getPointerTo()));
	SSA* stackAddr = Builder->CreateCall(StackSaveIntrinsic);
	SSA* GEP = Builder->CreateInBoundsGEP(DefinedTypes["byte*"].Type, jumpBuffer, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(64, 2, false)));
	Builder->CreateStore(stackAddr, GEP);
	IType.clear(); IArg.clear();
	IType.push_back(DefinedTypes["byte*"].Type);
	IArg.push_back(Builder->CreateBitCast(jumpBuffer, IType[0]));
	SSA* setjmpcall = Builder->CreateCall(SetJumpIntrinsic, llvm::ArrayRef<SSA*>(IArg));
	TypeAnnotation[setjmpcall] = DefinedTypes["dword"];
	return setjmpcall;
}

SSA* LongJumpExpression::Render() {
  llvm::Function *LongJumpIntrinsic = llvm::Intrinsic::getDeclaration(GlobalModule.get(), llvm::Intrinsic::eh_sjlj_longjmp);
  LongJumpIntrinsic->addFnAttr(llvm::Attribute::AlwaysInline);
  SSA *jumpBuffer = JumpBuffer->Render();
  std::vector<llvm::Type *> IType;
  std::vector<SSA *> IArg;
  IType.push_back(DefinedTypes["byte*"].Type);
  IArg.push_back(Builder->CreateBitCast(jumpBuffer, IType[0]));
	Builder->CreateCall(LongJumpIntrinsic, llvm::ArrayRef<SSA*>(IArg));
  SSA* R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false));
  TypeAnnotation[R] = DefinedTypes["dword"];
  return R;
}

SSA *SizeofExpression::Render() {
	SSA *value = Sized->Render();
	XLSType type = TypeAnnotation[value];
	SSA *R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, type.Size / 8, false));
	TypeAnnotation[R] = DefinedTypes["dword"];
	return R;
}

SSA *TypeofExpression::Render() {
	SSA *value = Typed->Render();
	XLSType type = TypeAnnotation[value];
	SSA *R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, type.UID, false));
	TypeAnnotation[R] = DefinedTypes["dword"];
	return R;
}

SSA *IfExpression::Render() {
	SSA *condition = Condition->Render();
	if (!condition) return nullptr;

	if (condition->getType() != llvm::Type::getInt1Ty(*GlobalContext))
		condition = Builder->CreateICmpNE(condition, llvm::Constant::getNullValue(condition->getType()), "xls_if_condition");
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

	llvm::PHINode *phiNode = Builder->CreatePHI(GetType(thenBranch).Type, 2, "xls_if_block");
	phiNode->addIncoming(thenBranch, thenBlock);
	phiNode->addIncoming(ImplicitCast(GetType(thenBranch), elseBranch), elseBlock);
	TypeAnnotation[phiNode] = GetType(thenBranch);
	return phiNode;
}

SSA *WhileExpression::Render() {
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	llvm::BasicBlock *conditionBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_while_check", function);
	llvm::BasicBlock *loopBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_loop", function);
	llvm::BasicBlock *afterBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_after_loop", function);
	BreakStack.push(afterBlock);
	ContinueStack.push(conditionBlock);


	if (!DoWhile)Builder->CreateBr(conditionBlock);
	else Builder->CreateBr(loopBlock);

	Builder->SetInsertPoint(conditionBlock);
	SSA *condition = Condition->Render();
	if (!condition) return nullptr;
	if (condition->getType() != llvm::Type::getInt1Ty(*GlobalContext))
		condition = Builder->CreateICmpNE( condition, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false)), "xls_while_condition");
	Builder->CreateCondBr(condition, loopBlock, afterBlock);

	Builder->SetInsertPoint(loopBlock);
	if (!Body->Render()) return nullptr;


	Builder->CreateBr(conditionBlock);
	Builder->SetInsertPoint(afterBlock);

	BreakStack.pop();
	ContinueStack.pop();

	SSA *R = llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*GlobalContext));
	TypeAnnotation[R] = DefinedTypes["dword"];
	return R;
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
		Builder->CreateStore(ImplicitCast(Type, definerSSA), alloca);

		AnnotatedValue stored;
		stored.Type = Type;
		stored.Value = alloca;
		AllonymousValues[variableName] = stored;
	}

	SSA *R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(Type.Size, 0, false));
	TypeAnnotation[R] = Type;
	return R;
}

SSA *StructDefinition::Render() {
  XLSType NEWType;
  StructData NEWStruct;
  NEWType.Name = Name;
  if (Mode == STRUCT_PRACTICAL) std::sort(Types.begin(), Types.end());
  std::vector<llvm::Type *> fields;
  for (unsigned i = 0; i < Types.size(); i++) {
    fields.push_back(Types[i].first.Type);
    NEWStruct.LiteralSize += Types[i].first.Size;
		NEWStruct.Fields[Types[i].second] = SDX(dword, XLSType)(i, Types[i].first);
  }
  NEWType.Type = llvm::StructType::create(*GlobalContext, llvm::ArrayRef<llvm::Type *>(fields), Name, Mode == STRUCT_PACKED);
  NEWStruct.Layout = (llvm::StructLayout *)GlobalLayout->getStructLayout((llvm::StructType *)NEWType.Type);
  NEWStruct.Size = NEWStruct.Layout->getSizeInBits();
  NEWType.Size = NEWStruct.Size;
  NEWType.IsStruct = true;
	NEWType.UID = CurrentUID++;
	NEWType.Structure = NEWStruct;
  DefinedTypes[Name] = NEWType;
  TypeMap[NEWType.Type] = NEWType;
  SSA *R = llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*GlobalContext));
  TypeAnnotation[R] = DefinedTypes["dword"];
  return R;
}

SSA *GlobalVariableNode::Render() {
	llvm::GlobalVariable *global = new llvm::GlobalVariable(*GlobalModule, Type.Type, false, llvm::GlobalValue::ExternalLinkage, 0, Name);
	global->setInitializer(llvm::ConstantInt::get(*GlobalContext, llvm::APInt(Type.Size, Value, false)));
	AnnotatedGlobal aGlobal;
	aGlobal.Type = Type;
	aGlobal.Value = global;
	GlobalValues[Name] = aGlobal;
	TypeAnnotation[global] = Type;
	return global;
}

llvm::Function *SignatureNode::Render() {
	std::vector<llvm::Type*> ArgumentType;
	for (uint i = 0; i < Arguments.size(); i++)
		ArgumentType.push_back(Arguments[i].second.Type);

	if (Variadic == VARIADIC_XLS) {
		Arguments.push_back(SDX(std::string, XLSType)("#hive", DefinedTypes["byte*"]));
		ArgumentType.push_back(DefinedTypes["byte*"].Type);
	}

	llvm::FunctionType *functionType = llvm::FunctionType::get(Type.Type, ArgumentType, Variadic == VARIADIC_C);

	llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, Name, GlobalModule.get());
	function->setCallingConv(Convention);
	if (!Type.UID) function->setDoesNotReturn();

	uint index = 0;
	for (llvm::Argument &argument : function->args()) {
		argument.setName(Arguments[index].first);
		ArgumentTypeAnnotation[function->getArg(index)] = Arguments[index].second;
		index++;
	}

	XLSFunctionInfo info;
	info.Variadic = Variadic;

	FunctionInfo[function] = info;
	ReturnTypeAnnotation[function] = Type;
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
	AllonymousLabels.clear();
	AnonymousLabels.clear();
	TypeAnnotation.clear();
	uint index = 0;
  for (llvm::Argument &argument : function->args()) {
		Alloca *alloca = createEntryBlockAlloca(function, argument.getName(), ArgumentTypeAnnotation[function->getArg(index)]);
		Builder->CreateStore(&argument, alloca);
		AnnotatedValue stored;
		stored.Type = ArgumentTypeAnnotation[function->getArg(index++)];
		stored.Value = alloca;
		AllonymousValues[std::string(argument.getName())] = stored;
	}

	if (function->isVarArg()) {
		Alloca *VAList = createEntryBlockAlloca(function, "variadic", DefinedTypes["valist"]);
		AnnotatedValue VAListStore;
		VAListStore.Type = DefinedTypes["valist"];
		VAListStore.Value = VAList;
		AllonymousValues["variadic"] = VAListStore;
		std::vector<llvm::Type*> IType;
		std::vector<SSA*> IArg;
		IType.push_back(DefinedTypes["byte*"].Type);
		IArg.push_back(Builder->CreateBitCast(VAList, IType[0]));
		llvm::Function* VAStart = llvm::Intrinsic::getDeclaration(GlobalModule.get(), llvm::Intrinsic::vastart);
		Builder->CreateCall(VAStart, llvm::ArrayRef<SSA*>(IArg));
		// Builder->CreateIntrinsic(llvm::Intrinsic::vastart, llvm::ArrayRef<llvm::Type*>(IType), llvm::ArrayRef<SSA*>(IArg));
	}

	if (SSA *returnValue = Body->Render()) {
		if (signature.GetType().Name != "void") Builder->CreateRet(ImplicitCast(signature.GetType(), returnValue));
		else Builder->CreateRetVoid();
		llvm::verifyFunction(*function);
		GlobalFPM->run(*function);
		return function;
	}

	function->eraseFromParent();
	return nullptr;
}

SSA* NullNode::Render() {
	return CodeError("Rendering a null node is invalid.");
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
		GlobalFPM->add(llvm::createTailCallEliminationPass());
		GlobalFPM->add(llvm::createSROAPass());
		GlobalFPM->add(llvm::createLoopVectorizePass());
		GlobalFPM->add(llvm::createVerifierPass());
	}

	GlobalFPM->doInitialization();

	Builder = MUQ(llvm::IRBuilder<>, *GlobalContext);

	// Type definitions.
	XLSType InvalidType;
	InvalidType.Size = 0;
	InvalidType.Type = llvm::Type::getInt1Ty(*GlobalContext);
	InvalidType.Name = "N/A";
	InvalidType.UID = CurrentUID++;

	DefinedTypes[InvalidType.Name] = InvalidType;
	XLSType InternalAddressSizeType;
	InternalAddressSizeType.Size = GlobalLayout->getPointerSizeInBits();
	InternalAddressSizeType.Type = llvm::Type::getInt64Ty(*GlobalContext);
	InternalAddressSizeType.Name = "#addrsize";
	InternalAddressSizeType.UID = CurrentUID++;
	DefinedTypes[InternalAddressSizeType.Name] = InternalAddressSizeType;

	XLSType DwordType, WordType, ByteType, BooleType, VoidType, BoolePtrType, VoidPtrType, BytePtrType;

	DwordType.Size = 32;
	DwordType.Type = llvm::Type::getInt32Ty(*GlobalContext);
	DwordType.Name = "dword";
	DwordType.UID = CurrentUID++;

	WordType.Size = 16;
	WordType.Type = llvm::Type::getInt16Ty(*GlobalContext);
	WordType.Name = "word";
	WordType.UID = CurrentUID++;

	ByteType.Size = 8;
	ByteType.Type = llvm::Type::getInt8Ty(*GlobalContext);
	ByteType.Name = "byte";
	ByteType.UID = CurrentUID++;

	BooleType.Size = 1;
	BooleType.Type = llvm::Type::getInt1Ty(*GlobalContext);
	BooleType.Name = "boole";
	BooleType.UID = CurrentUID++;

	VoidType.Size = 0;
	VoidType.Type = llvm::Type::getVoidTy(*GlobalContext);
	VoidType.Name = "void";
	VoidType.UID = CurrentUID++;

	BoolePtrType.Size = GlobalLayout->getPointerSizeInBits();
	BoolePtrType.Type = llvm::Type::getInt1PtrTy(*GlobalContext);
	BoolePtrType.Name = "boole*";
	BoolePtrType.Dereference = "boole";
	BoolePtrType.IsPointer = true;
	BoolePtrType.UID = CurrentUID++;

	BytePtrType.Size = GlobalLayout->getPointerSizeInBits();
	BytePtrType.Type = llvm::Type::getInt8PtrTy(*GlobalContext);
	BytePtrType.Name = "byte*";
	BytePtrType.Dereference = "byte";
	BytePtrType.IsPointer = true;
	BytePtrType.UID = CurrentUID++;

	VoidPtrType = BytePtrType;
	VoidPtrType.Name = "void*";
	VoidPtrType.Dereference = "void";
	VoidPtrType.UID = CurrentUID++;

	// Signed types
	XLSType SdwordType, SwordType, SbyteType;

	SdwordType.Size = 32;
	SdwordType.Type = llvm::Type::getInt32Ty(*GlobalContext);
	SdwordType.Signed = true;
	SdwordType.Name = "sdword";
	SdwordType.UID = CurrentUID++;

	SwordType.Size = 16;
	SwordType.Type = llvm::Type::getInt16Ty(*GlobalContext);
	SwordType.Signed = true;
	SwordType.Name = "sword";
	SwordType.UID = CurrentUID++;

	SbyteType.Size = 8;
	SbyteType.Type = llvm::Type::getInt8Ty(*GlobalContext);
	SbyteType.Signed = true;
	SbyteType.Name = "sbyte";
	SbyteType.UID = CurrentUID++;


	// Label type
	XLSType LabelType;
	LabelType.Size = GlobalLayout->getPointerSizeInBits();
	LabelType.Type = llvm::Type::getInt8PtrTy(*GlobalContext);
	LabelType.Name = "label&";
	LabelType.Dereference = "void";
	LabelType.IsPointer = true;
	LabelType.IsLabel = true;
	LabelType.UID = CurrentUID++;

	DefinedTypes[DwordType.Name] = DwordType;
	DefinedTypes[WordType.Name] = WordType;
	DefinedTypes[ByteType.Name] = ByteType;
	DefinedTypes[BooleType.Name] = BooleType;
	DefinedTypes[VoidType.Name] = VoidType;
	DefinedTypes[BoolePtrType.Name] = BoolePtrType;
	DefinedTypes[BytePtrType.Name] = BytePtrType;
	DefinedTypes[VoidPtrType.Name] = VoidPtrType;

	DefinedTypes[SdwordType.Name] = SdwordType;
	DefinedTypes[SwordType.Name] = SwordType;
	DefinedTypes[SbyteType.Name] = SbyteType;

	DefinedTypes[LabelType.Name] = LabelType;

	TypeMap[DwordType.Type] = DwordType;
	TypeMap[WordType.Type] = WordType;
	TypeMap[ByteType.Type] = ByteType;
	TypeMap[BooleType.Type] = BooleType;
	TypeMap[VoidType.Type] = VoidType;
	TypeMap[BoolePtrType.Type] = BoolePtrType;
	TypeMap[BytePtrType.Type] = BytePtrType;

	XLSType JmpBufType;
	JmpBufType.Size = 5 * GlobalLayout->getPointerSizeInBits();
	JmpBufType.Type = llvm::Type::getIntNTy(*GlobalContext, JmpBufType.Size);
	JmpBufType.Name = "jmpbuf";
	JmpBufType.UID = CurrentUID++;

	DefinedTypes[JmpBufType.Name] = JmpBufType;
	TypeMap[JmpBufType.Type] = JmpBufType;

	// C Variadic type
	XLSType VariadicType;
	std::vector<llvm::Type*> VFields;

	if (GlobalTriple->isX86() && GlobalLayout->getPointerSizeInBits() == 64) {
		// Use variadic ABI for AMD64
		// http://refspecs.linuxbase.org/elf/x86_64-abi-0.21.pdf
		VFields.push_back(llvm::Type::getInt32Ty(*GlobalContext));
		VFields.push_back(llvm::Type::getInt32Ty(*GlobalContext));
		VFields.push_back(llvm::Type::getInt8PtrTy(*GlobalContext));
		VFields.push_back(llvm::Type::getInt8PtrTy(*GlobalContext));
	} else if (GlobalTriple->isSystemZ()) {
		// Use variadic ABI for SystemZ
		VFields.push_back(llvm::Type::getInt32Ty(*GlobalContext));
		VFields.push_back(llvm::Type::getInt32Ty(*GlobalContext));
		VFields.push_back(llvm::Type::getInt8PtrTy(*GlobalContext));
		VFields.push_back(llvm::Type::getInt8PtrTy(*GlobalContext));
	} else if (GlobalTriple->getArch() == llvm::Triple::ArchType::hexagon) {
		// Use variadic ABI for Hexagon
		VFields.push_back(llvm::Type::getInt8PtrTy(*GlobalContext));
		VFields.push_back(llvm::Type::getInt8PtrTy(*GlobalContext));
		VFields.push_back(llvm::Type::getInt8PtrTy(*GlobalContext));
	} else {
		// Use generic VALIST structure
		VFields.push_back(llvm::Type::getInt8PtrTy(*GlobalContext));
	}

	VariadicType.Type = llvm::StructType::create(*GlobalContext, llvm::ArrayRef<llvm::Type*>(VFields), "valist");
	VariadicType.Size = VariadicType.Type->getScalarSizeInBits();
	VariadicType.Name = "valist";
	VariadicType.IsStruct = true;
	VariadicType.UID = CurrentUID++;

	DefinedTypes[VariadicType.Name] = VariadicType;
	TypeMap[VariadicType.Type] = VariadicType;
}

void HandleImplementation() {
	if (UQP(FunctionNode) functionNode = ParseImplementation()) {
		if (llvm::Function* functionIR = functionNode->Render()) {
		}
	} else GetNextToken();
}

void HandleOperatorDefinition() {
  if (UQP(FunctionNode) functionNode = ParseOperatorDefinition()) {
    if (llvm::Function *functionIR = functionNode->Render()) {
    }
  } else GetNextToken();
}

void HandleExtern() {
	if (UQP(SignatureNode) signature = ParseExtern()) {
		if (llvm::Function* signatureIR = signature->Render()) {
			FunctionSignatures[signature->GetName()] = std::move(signature);
		}
	} else GetNextToken();
}

void HandleGlobal() {
	if (UQP(Statement) global = ParseGlobalVariable(DefinedTypes[CurrentIdentifier])) {
		if (SSA *globalIR = global->Render()) {
		}
	}
}

void HandleStruct() {
	if (UQP(Statement) definition = ParseStruct()) {
		definition->Render();
	} else GetNextToken();
}

void HandleTypedef() {
	if (!ParseTypedef()) GetNextToken();
}

void HandleUnboundedExpression() {
	if (UQP(FunctionNode) functionNode = ParseUnboundedExpression()) {
		functionNode->Render();
	} else GetNextToken();
}

bool CheckTypeDefined(std::string name) {
	if (DefinedTypes.find(name) != DefinedTypes.end()) return true;
	std::string dereference = name;
	if (name.back() == '*') {
		dereference.pop_back();
		if (!CheckTypeDefined(dereference)) return false;
		XLSType NEWType;
		NEWType.Name = name;
		NEWType.Type = DefinedTypes[dereference].Type->getPointerTo();
		NEWType.IsPointer = true;
		NEWType.Signed = false;
		NEWType.Size = GlobalLayout->getPointerSizeInBits();
		NEWType.Dereference = dereference;
		NEWType.UID = CurrentUID++;
		DefinedTypes[name] = NEWType;
		TypeMap[NEWType.Type] = NEWType;
		return true;
	}

	if (name == "fn:") {
		std::string typeName;
		std::vector<XLSType> constituentTypes;
		typeName = "fn:(";
		GetNextToken();
		if (CurrentToken.Value != '(') return false;
		GetNextToken();
		while (CurrentToken.Value != ')') {
			if (CurrentToken.Type != LEXEME_IDENTIFIER) return false;
			if (!CheckTypeDefined(CurrentIdentifier)) return false;
			typeName += CurrentIdentifier;
			constituentTypes.push_back(DefinedTypes[CurrentIdentifier]);
			GetNextToken();
			if (CurrentToken.Value == ',') {
				GetNextToken();
				continue;
			}
		}
		// '('
		typeName += "):";
		GetNextToken();
		if (CurrentToken.Value != ':') return false;
		GetNextToken();
		if (CurrentToken.Type != LEXEME_IDENTIFIER) return false;
		if (!CheckTypeDefined(CurrentIdentifier)) return false;
		typeName += CurrentIdentifier;
		constituentTypes.push_back(DefinedTypes[CurrentIdentifier]);

		CurrentIdentifier = typeName;
		if (CheckTypeDefined(typeName)) return true;
		XLSType NEWType;
		NEWType.Name = typeName;
		std::vector<llvm::Type*> TypeArguments;
		XLSType rType = constituentTypes.back();
		constituentTypes.pop_back();
		for (uint i = 0; i < constituentTypes.size(); i++)
			TypeArguments.push_back(constituentTypes[i].Type);
		llvm::FunctionType* functionType = llvm::FunctionType::get(rType.Type, TypeArguments, false);
		llvm::PointerType* fpType = llvm::PointerType::get(functionType, 0);
		NEWType.Type = fpType;
		NEWType.Size = GlobalLayout->getPointerSizeInBits();
		NEWType.IsPointer = true;
		NEWType.IsFP = true;
		NEWType.Dereference = "#addrsize";
		constituentTypes.insert(constituentTypes.begin(), rType);
		NEWType.FPData = constituentTypes;
		NEWType.UID = CurrentUID++;
		DefinedTypes[typeName] = NEWType;
		return true;
	}
	return false;
}

bool DefineFPType(std::string function, XLSType* outtype) {
	if (FunctionSignatures.find(function) == FunctionSignatures.end()) return false;
  llvm::Function *pointed = getFunction(function);
  XLSType FPType;
	FPType.Name = "fn:(";
  FPType.Type = pointed->getType();
  FPType.Size = GlobalLayout->getPointerSizeInBits();
  FPType.Dereference = "#addrsize";
  FPType.IsPointer = true;
	FPType.IsFP = true;
	FPType.FPData.push_back(ReturnTypeAnnotation[pointed]);
	for (uint i = 0; i < pointed->arg_size(); i++) {
		FPType.Name += ArgumentTypeAnnotation[pointed->getArg(i)].Name;
		FPType.FPData.push_back(ArgumentTypeAnnotation[pointed->getArg(i)]);
		if (i + 1 != pointed->arg_size()) FPType.Name += ", ";
	}
	FPType.Name += "):" + FPType.FPData[0].Name;
	FPType.UID = CurrentUID++;
  DefinedTypes[FPType.Name] = FPType;
  TypeMap[FPType.Type] = FPType;
	*outtype = FPType;
	return true;
}


extern "C" dword putchard(dword X) {
	fputc((char)X, stderr);
	return 0;
}

extern "C" dword printd(dword X) {
	fprintf(stderr, "%u\n", X);
	return 0;
}
