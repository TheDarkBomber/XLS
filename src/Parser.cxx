#include "Parser.hxx"
#include "Variables.hxx"
#include "Type.hxx"
#include "Lexer.hxx"
#include "XLiSp.hxx"
#include "Debug.hxx"
#include "colours.def.h"
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/Triple.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DIBuilder.h>
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

std::queue<TokenContext> TokenStream;
Token CurrentToken;

Token GetNextToken() {
	if (!TokenStream.empty()) {
		TokenContext t = TokenStream.front();
		CurrentIdentifier = t.Identifier;
		StringLiteral = t.StringLiteral;
		CurrentOperator = t.Operator;
		CurrentInteger = t.IntegerLiteral;
		CurrentToken = t.Value;
		ExtraData = t.ExtraData;
		TokenStream.pop();
		return t.Value;
	}
	return CurrentToken = GetToken();
}

std::map<std::string, Precedence> BinaryPrecedence;

UQP(llvm::LLVMContext) GlobalContext;
UQP(llvm::IRBuilder<>) Builder;
UQP(llvm::Module) GlobalModule;
UQP(llvm::legacy::FunctionPassManager) GlobalFPM;

std::map<std::string, UQP(SignatureNode)> FunctionSignatures;
std::map<std::string, llvm::BasicBlock*> AllonymousLabels;

std::map<llvm::Function*, XLSFunctionInfo> FunctionInfo;

std::vector<llvm::BasicBlock*> AnonymousLabels;
std::string CurrentLabelIdentifier = "current";

std::stack<llvm::BasicBlock*> BreakStack;
std::stack<llvm::BasicBlock*> ContinueStack;

std::map<std::string, std::vector<MacroArgument>> Macros;

void AlertError(const char *error) {
	llvm::errs() <<
		COLOUR_YELLOW <<
		"R" << CurrentLocation.Row << "C" << CurrentLocation.Column << ": " <<
		COLOUR_RED <<
		"Error: " << error << COLOUR_RED_BOLD <<
		COLOUR_END << "\n";
}

void AlertWarning(const char *warning) {
	llvm::errs() <<
		COLOUR_YELLOW <<
		"R" << CurrentLocation.Row << "C" << CurrentLocation.Column << ": " <<
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
	if (Macros.find(identifier) != Macros.end()) return ParseMacro(identifier);
	if (CheckTypeDefined(identifier))
		return ParseDeclaration(DefinedTypes[CurrentIdentifier]);
	GetNextToken();
	UQP(Expression) offset;
	if (CurrentToken.Value == '[') {
		GetNextToken();
		offset = ParseExpression(isVolatile);
		if (!offset) return nullptr;
		if (CurrentToken.Value != ']') return ParseError("Expected close square parenthesis in dereference offset.");
		GetNextToken();
	}
	if (CurrentToken.Value == '.') {
		GetNextToken();
		if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected field name.");
		std::string field = CurrentIdentifier;
		GetNextToken();
		UQP(Expression) fieldOffset = nullptr;
		if (CurrentToken.Value == '[') {
			GetNextToken();
			fieldOffset = ParseExpression(isVolatile);
			if (!fieldOffset) return nullptr;
			if (CurrentToken.Value != ']') return ParseError("Expected close square parenthesis in field offset.");
			GetNextToken();
		}
		return MUQ(VariableExpression, identifier, isVolatile, false, std::move(offset), field, std::move(fieldOffset));
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
	case LEXEME_COUNTOF:
		return ParseCountof();
	case LEXEME_MEMSET:
		return ParseMemset();
	case LEXEME_MEMCOPY:
		return ParseMemcopy();
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
	case LEXEME_RAW:
		return ParseRaw();
	case LEXEME_VOLATILE:
		GetNextToken();
		return ParseExpression(true);
	default:
		switch (CurrentToken.Value) {
		case '(': // ')'
			return ParseParenthetical();
		case '{': // '}'
			return ParseBlock();
		case '$':
			return ParseXLiSp();
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
		LHS = ParsePostfix(std::move(LHS));
		Precedence tokenPrecedence = GetTokenPrecedence();
		if (tokenPrecedence <= precedence) return std::move(LHS);

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

UQP(Expression) ParsePostfix(UQP(Expression) LHS) {
	if (CurrentToken.Type == LEXEME_CHARACTER && CurrentToken.Value == '.') {
		GetNextToken();
		if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier after dot notation.");
		std::string identifier = CurrentIdentifier;
		GetNextToken();
		return MUQ(FieldAccessExpression, identifier, std::move(LHS));
	}

	return LHS;
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

UQP(Expression) ParseCountof() {
	if (CurrentToken.Subtype == LEXEME_ELSE) return ParseSetCountof();
	GetNextToken();
	if (CurrentToken.Type == LEXEME_IDENTIFIER && CheckTypeDefined(CurrentIdentifier)) {
		std::string type = CurrentIdentifier;
		GetNextToken();
		return MUQ(DwordExpression, GetCountof(DefinedTypes[type]));
	}
	UQP(Expression) typed = ParseExpression();
	return MUQ(CountofExpression, std::move(typed));
}

UQP(Expression) ParseSetCountof() {
	GetNextToken();
	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis after setcountof");
	GetNextToken();
	UQP(Expression) counted = ParseExpression();
	if (CurrentToken.Value != ',') return ParseError("Expected comma in setcountof");
	GetNextToken();
	UQP(Expression) newCount = ParseExpression();
	if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis to terminate setcountof");
	GetNextToken();
	return MUQ(SetCountofExpression, std::move(counted), std::move(newCount));
}

UQP(Expression) ParseMemset() {
	GetNextToken();
	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis after memset");
	GetNextToken();
	UQP(Expression) ptr = ParseExpression();
	if (CurrentToken.Value != ',') return ParseError("Expected comma in memset");
	GetNextToken();
	UQP(Expression) value = ParseExpression();
	if (CurrentToken.Value == ')') {
		GetNextToken();
		return MUQ(MemsetExpression, std::move(ptr), std::move(value));
	}
	if (CurrentToken.Value != ',') return ParseError("Expected close parenthesis or comma in memset");
	GetNextToken();
	// '('
	UQP(Expression) length = ParseExpression();
	if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis to terminate memset");
	GetNextToken();
	return MUQ(MemsetExpression, std::move(ptr), std::move(value), std::move(length));
}

UQP(Expression) ParseMemcopy() {
	bool overlap = (CurrentToken.Subtype == LEXEME_ELSE);
	GetNextToken();
	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis after memcopy");
	GetNextToken();
	UQP(Expression) destination = ParseExpression();
	if (CurrentToken.Value != ',') return ParseError("Expected comma in memcopy");
	GetNextToken();
	UQP(Expression) source = ParseExpression();
	if (CurrentToken.Value == ')') {
		GetNextToken();
		return MUQ(MemcopyExpression, std::move(destination), std::move(source), nullptr, overlap);
	}
	if (CurrentToken.Value != ',') return ParseError("Expected close parenthesis or comma in memcopy");
	GetNextToken();
	// '('
	UQP(Expression) length = ParseExpression();
	if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis to terminate memcopy");
	GetNextToken();
	return MUQ(MemcopyExpression, std::move(destination), std::move(source), std::move(length), overlap);
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
		UQP(Expression) dsize = nullptr;
		bool sizeIsDynamic = false;
		if (CurrentToken.Type != LEXEME_INTEGER) { dsize = ParseExpression(); sizeIsDynamic = true; }
		UQP(MutableArrayExpression) result = MUQ(MutableArrayExpression, dsize ? 0 : CurrentInteger, type, std::move(dsize));
		if (!sizeIsDynamic) GetNextToken();
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

UQP(Expression) ParseMacro(std::string macro) {
	GetNextToken();
	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis to begin macro call.");
	auto macrarguments = Macros[macro];
	std::vector<UQP(Expression)> values;
	for (MacroArgument M : macrarguments) {
		GetNextToken();
		switch (M.GetType()) {
		case MACRO_ARGUMENT_INTEGER:
			if (CurrentToken.Type != LEXEME_INTEGER) return ParseError("Expected integer in macro call.");
			values.push_back(ParseDwordExpression());
			break;
		case MACRO_ARGUMENT_STRING:
			if (CurrentToken.Type != LEXEME_STRING) return ParseError("Expected string in macro call.");
			values.push_back(ParseStringExpression());
			break;
		case MACRO_ARGUMENT_EXPRESSION:
			values.push_back(ParseExpression());
			break;
		case MACRO_ARGUMENT_TYPENAME:
			if (!CheckTypeDefined(CurrentIdentifier)) return ParseError("Expected type name in macro call.");
		case MACRO_ARGUMENT_IDENTIFIER:
			values.push_back(std::move(MUQ(VariableExpression, CurrentIdentifier)));
			GetNextToken();
			break;
		default: return ParseError("Unimplemented macro argument type.");
		}
		if (CurrentToken.Value == ',') continue;
		if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis to end macro call.");
	}
	if (values.size() != macrarguments.size()) return ParseError("Count of macro metatypes and values do not match.");
	GetNextToken();
	return MUQ(MacroExpression, macro, std::move(values), macrarguments);
}

UQP(Expression) ParseXLiSp() {
	GetNextToken();
	if (CurrentToken.Value != '(') return ParseError("Expected open parenthesis to begin XLiSp expression");
	std::queue<TokenContext> stream;
	dword nest = 0;
	while (GetNextToken().Value != ')' || nest > 0) {
		TokenContext t;
		t.Value = CurrentToken;
		t.CharacterLiteral = t.Value.Value;
		t.IntegerLiteral = CurrentInteger;
		t.StringLiteral = StringLiteral;
		t.Identifier = CurrentIdentifier;
		t.Operator = CurrentOperator;
		stream.push(t);
		if (t.Value.Value == '(') nest++;
		if (t.Value.Value == ')') nest--;
	}
	GetNextToken();
	return MUQ(XLiSpExpression, stream);
}

UQP(Expression) ParseRaw() {
	SSA* rendered = (SSA*)ExtraData;
	GetNextToken();
	return MUQ(RawExpression, rendered);
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

UQP(Statement) ParseFuncdef() {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier after funcdef.", 0, 0);
	if (CMP(CurrentIdentifier, "macro")) return ParseFuncdefMacro();
	return ParseError("Unknown funcdef operand.", 0, 0);
}

UQP(Statement) ParseFuncdefMacro() {
	GetNextToken();
	if (CurrentToken.Type != LEXEME_IDENTIFIER) return ParseError("Expected identifier after funcdef macro.", 0, 0);
	std::string macro = CurrentIdentifier;
	GetNextToken();
	if (CurrentToken.Type != LEXEME_CHARACTER || CurrentToken.Value != '(') return ParseError("Expected open parenthesis in funcdef macro.", 0, 0);
	auto macrarguments = std::vector<MacroArgument>();
	for (;;) {
		GetNextToken();
		if (CurrentToken.Type != LEXEME_IDENTIFIER && CurrentToken.Type != LEXEME_VARIADIC)
			return ParseError("Expected identifier or variadic in funcdef macro.", 0, 0);
		// TODO: Handle the variadic case.
		if (CurrentIdentifier == "integer") macrarguments.push_back(MACRO_ARGUMENT_INTEGER);
		else if (CurrentIdentifier == "string") macrarguments.push_back(MACRO_ARGUMENT_STRING);
		else if (CurrentIdentifier == "expression") macrarguments.push_back(MACRO_ARGUMENT_EXPRESSION);
		else if (CurrentIdentifier == "typename") macrarguments.push_back(MACRO_ARGUMENT_TYPENAME);
		else if (CurrentIdentifier == "identifier") macrarguments.push_back(MACRO_ARGUMENT_IDENTIFIER);
		else return ParseError("Unknown metatype for funcdef macro.", 0, 0);

		GetNextToken();
		if (CurrentToken.Value == ',') continue;
		if (CurrentToken.Value != ')') return ParseError("Expected close parenthesis in funcdef macro.", 0, 0);
		else break;
	}
	Macros[macro] = macrarguments;
	GetNextToken();
	return MUQ(NullNode);
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
	return MUQ(SignatureNode, functionName, std::move(argumentNames), DefinedTypes["dword"], CurrentLocation, false, VARIADIC_NONE, llvm::CallingConv::Fast, true, precedence);
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

	bool internal = false;
	if (CurrentIdentifier == "intern") {
		internal = true;
		GetNextToken();
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

	return MUQ(SignatureNode, functionName, std::move(argumentNames), type, CurrentLocation, internal, variadic, convention);
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
	static dword times = 0;
	if (UQP(Expression) expression = ParseExpression()) {
		UQP(SignatureNode) signature = MUQ(SignatureNode, "__mistakeman" + std::to_string(times), VDX(std::string, XLSType)(), DefinedTypes["dword"], CurrentLocation, true);
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

Alloca* createEntryBlockAlloca(llvm::Function *function, llvm::StringRef variableName, XLSType type = DefinedTypes["dword"], SSA* size = nullptr) {
	llvm::IRBuilder<> apiobuilder(&function->getEntryBlock(), function->getEntryBlock().begin());
	return apiobuilder.CreateAlloca(type.Type, size, variableName);
}

SSA *DwordExpression::Render() {
	EMIT_DEBUG;
	SSA *R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, Value, false));
	TypeAnnotation[R] = DefinedTypes["dword"];
	return R;
}

SSA *CharacterExpression::Render() {
	EMIT_DEBUG;
	SSA *R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(8, Value, false));
	TypeAnnotation[R] = DefinedTypes["byte"];
	return R;
}

SSA *StringExpression::Render() {
	EMIT_DEBUG;
	std::vector<llvm::Constant*> elements(Value.size());
	for (unsigned i = 0; i < Value.size(); i++) {
		elements[i] = llvm::ConstantInt::get(DefinedTypes["byte"].Type, Value[i]);
	}

	if (Terminator == ST_NULL) elements.push_back(llvm::ConstantInt::get(DefinedTypes["byte"].Type, 0x00));

	llvm::ArrayType* strType = llvm::ArrayType::get(DefinedTypes["byte"].Type, elements.size());

	llvm::GlobalVariable* global = new llvm::GlobalVariable(*GlobalModule, strType, !Mutable, llvm::GlobalValue::PrivateLinkage, llvm::ConstantArray::get(strType, elements), "xls_string");
	SSA* StringPtr = llvm::ConstantExpr::getBitCast(global, DefinedTypes["byte*"].Type);
	SSA* Length = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(DefinedTypes["#addrsize"].Size, Value.size(), false));

	SSA* R = ZeroSSA(DefinedTypes["byte%"]);
	R = Builder->CreateInsertValue(R, StringPtr, llvm::ArrayRef<unsigned>(RANGED_POINTER_VALUE));
	R = Builder->CreateInsertValue(R, Length, llvm::ArrayRef<unsigned>(RANGED_POINTER_COUNTOF));
	TypeAnnotation[R] = DefinedTypes["byte%"];
	return R;
}

SSA* MutableArrayExpression::Render() {
	EMIT_DEBUG;
	if (!CheckTypeDefined(Type.Name + "*")) return nullptr;
	if (!CheckTypeDefined(Type.Name + "%")) return nullptr;
	SSA* Length;
	if (!DynamicSize) Length = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(DefinedTypes["#addrsize"].Size, Size, false));
	else Length = Cast(DefinedTypes["#addrsize"], DynamicSize->Render());
	SSA* Array = !DynamicSize ? createEntryBlockAlloca(Builder->GetInsertBlock()->getParent(), "xls_mutable_array", Type, Length) : Builder->CreateAlloca(Type.Type, Length, "xls_mutable_array");

	SSA* R = ZeroSSA(DefinedTypes[Type.Name + "%"]);
	R = Builder->CreateInsertValue(R, Array, llvm::ArrayRef<unsigned>(RANGED_POINTER_VALUE));
	R = Builder->CreateInsertValue(R, Length, llvm::ArrayRef<unsigned>(RANGED_POINTER_COUNTOF));
	TypeAnnotation[R] = DefinedTypes[Type.Name + "%"];
	return R;
}

SSA* VariableExpression::Render() {
	EMIT_DEBUG;
	if (AllonymousValues.find(Name) == AllonymousValues.end()) {
		if (FunctionSignatures.find(Name) != FunctionSignatures.end()) {
			llvm::Function *pointed = getFunction(Name);
			XLSType FPType;
			if (!DefineFPType(Name, &FPType)) return nullptr;
			TypeAnnotation[pointed] = FPType;
			return pointed;
		} else return CodeError("Reference to undeclared variable.");
	}

	XLSVariable variable = AllonymousValues[Name];
	if (Dereference) IndexVariable(variable, ZeroSSA(DefinedTypes["dword"]), Volatile);

	if (FieldOffset) {
		variable = FetchVirtualVariable(this);
		SSA* offset = FieldOffset->Render();
		if (!offset) return nullptr;
		return IndexVariable(variable, offset, Volatile);
	}

	if (Field != "") {
		if (!variable.Type.IsStruct) return nullptr;
		if (variable.Type.Structure.Fields.find(Field) == variable.Type.Structure.Fields.end()) return CodeError("Unknown field.");

		if (variable.Type.IsPointer || variable.Type.IsRangedPointer) variable = DemoteVariable(variable);

		dword fieldOffset = variable.Type.Structure.Fields[Field].first;
		XLSType fieldType = variable.Type.Structure.Fields[Field].second;
		SSA* fieldOffsetSSA = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, fieldOffset, false));
		return IndexVariableField(variable, fieldType, fieldOffsetSSA, Volatile);
	}

	if (Offset) {
		SSA* offset = Offset->Render();
		if (!offset) return nullptr;
		return IndexVariable(variable, offset, Volatile);
	}

	return ReadVariable(variable, Volatile);
}

SSA* CastExpression::Render() {
	EMIT_DEBUG;
	SSA* toCast = Value->Render();
	if (!toCast) return nullptr;
	SSA* R = Cast(Type, toCast);
	TypeAnnotation[R] = Type;
	return R;
}

SSA* PtrRHS(SSA* left, SSA* right) {
	return Cast(GetType(left), Builder->CreateMul(right, llvm::ConstantInt::get(*GlobalContext, llvm::APInt(GetType(right).Size, DefinedTypes[GetType(left).Dereference].Size / 8))));
}

SSA* BinaryExpression::Render() {
	EMIT_DEBUG;
	if (CMP("=", Operator)) {
		VariableExpression* LAssignment = static_cast<VariableExpression*>(LHS.get());
		if (!LAssignment) return CodeError("Assignment on fire.");
		SSA* value = RHS->Render();
		if (!value) return nullptr;
		if (!GetType(value).UID) value = llvm::PoisonValue::get(value->getType());
		if (AllonymousValues.find(LAssignment->GetName()) == AllonymousValues.end())
			return CodeError("Unknown variable name.");

		XLSVariable variable = AllonymousValues[LAssignment->GetName()];
		if (LAssignment->GetFieldOffset() != nullptr) {
			variable = FetchVirtualVariable(LAssignment);
			SSA* offset = LAssignment->GetFieldOffset()->Render();
			if (!offset) return nullptr;
			return ExdexVariable(value, variable, offset, Volatile);
		}
		if (LAssignment->GetField() != "") {
			if (!variable.Type.IsStruct) return nullptr;
			if (variable.Type.Structure.Fields.find(LAssignment->GetField()) == variable.Type.Structure.Fields.end()) return CodeError("Unknown field to assign.");

			if (variable.Type.IsPointer || variable.Type.IsRangedPointer) variable = DemoteVariable(variable);

			dword fieldOffset = variable.Type.Structure.Fields[LAssignment->GetField()].first;
			XLSType fieldType = variable.Type.Structure.Fields[LAssignment->GetField()].second;
			SSA* fieldOffsetSSA = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, fieldOffset, false));
			return ExdexVariableField(value, variable, fieldType, fieldOffsetSSA, Volatile);
		}
		if (LAssignment->GetOffset())
			return ExdexVariable(value, variable, LAssignment->GetOffset()->Render(), Volatile);

		if (LAssignment->IsDereference())
			return ExdexVariable(value, variable, ZeroSSA(DefinedTypes["dword"]), Volatile);

		return WriteVariable(value, variable, Volatile);
	}

	if (CMP("|>", Operator)) {
		VariableExpression *RFunction = static_cast<VariableExpression*>(RHS.get());
		if (!RFunction) return CodeError("Pipe on fire.");
		SSA *lvalue = LHS->Render();
		if (!lvalue) return nullptr;
		if (CheckTypeDefined(RFunction->GetName())) {
			return Cast(DefinedTypes[RFunction->GetName()], lvalue);
		}
		llvm::Function *pipe = getFunction(RFunction->GetName());
		if (!pipe) return CodeError("Unknown function name in pipe.");
		if (pipe->arg_size() != 1) return CodeError("Can only pipe into function with one argument.");
		llvm::Argument *piped = pipe->getArg(0);
		llvm::CallInst *call = Builder->CreateCall(pipe, Cast(GetType(piped), lvalue), "xls_pipe");
		call->setCallingConv(pipe->getCallingConv());
		call->setAttributes(pipe->getAttributes());
		return call;
	}

#define RCAST Cast(GetType(left), right)
#define RPTR left->getType()->isPointerTy() ? PtrRHS(left, right) : right
#define RPCAST left->getType()->isPointerTy() ? PtrRHS(left, RCAST) : RCAST
	SSA* R;
#define RET(V) do { R = V; TypeAnnotation[R] = GetType(left); return R; } while(0)
	SSA* left = LHS->Render();
	SSA* right;
	if (Operator != "&&" && Operator != "||") {
		right = RHS->Render();
		if (!right) return nullptr;
	}
	if (!left) return nullptr;
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
	if (GetType(left).IsPointer)
		RET(Builder->CreateInBoundsGEP(DefinedTypes[GetType(left).Dereference].Type, left, right));
	RET(Builder->CreateAdd(left, RPCAST, "xls_add"));
 Operators_minus:
	if (GetType(left).IsPointer)
		RET(Builder->CreateInBoundsGEP(DefinedTypes[GetType(left).Dereference].Type, left, Builder->CreateMul(right, llvm::ConstantInt::get(GetType(right).Type, llvm::APInt(GetType(right).Size, -1, false)))));
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
	RET(CreateLogicalAnd(left, std::move(RHS), false));
 Operators_logical_or:
	RET(CreateLogicalAnd(left, std::move(RHS), true));
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
	EMIT_DEBUG;
	if(CMP("&", Operator)) {
		VariableExpression *LAssignment = static_cast<VariableExpression*>(Operand.get());
		if (!LAssignment) return CodeError("Address-of operation on fire.");
		return AddrVariable(LAssignment->GetName());
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
	operand = Cast(DefinedTypes["byte"], operand);
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

SSA* CallExpression::Render() {
	EMIT_DEBUG;
	llvm::Function* called = getFunction(Called);
	if (!called) {
		if (AllonymousValues.find(Called) == AllonymousValues.end())
			return CodeError("Call to undeclared function.");

		XLSVariable variable = AllonymousValues[Called];
		if (!variable.Type.IsFP) return CodeError("Call to non-function pointer.");
		llvm::LoadInst* FP = Builder->CreateLoad(variable.Type.Type, variable.Value, "xls_fp_load");

		if (variable.Type.FPData.size() != Arguments.size() + 1) return CodeError("Argument call size mismatch with type argument size.");

		std::vector<SSA*> FPArguments;
		for (uint i = 0; i < Arguments.size(); i++)
			FPArguments.push_back(Cast(variable.Type.FPData[i + 1], Arguments[i]->Render()));

		std::vector<llvm::Type*> FPParams;
		for (uint i = 1; i < variable.Type.FPData.size(); i++)
			FPParams.push_back(variable.Type.FPData[i].Type);


		llvm::FunctionType* FPType = llvm::FunctionType::get(variable.Type.FPData[0].Type, llvm::ArrayRef<llvm::Type*>(FPParams), false);
		llvm::CallInst* callInstance = Builder->CreateCall(FPType, FP, FPArguments, "xls_fp_call");
		if (variable.Type.FPData[0].UID == DefinedTypes["void"].UID) callInstance->setName("");
		TypeAnnotation[callInstance] = variable.Type.FPData[0];
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
			XLSVariable HiveStore = AllonymousValues["#hive"];
			SSA* Hive = Builder->CreateLoad(HiveStore.Type.Type, HiveStore.Value, "xls_pass_hive");
			ArgumentVector.push_back(Hive);
			break;
		}
		ArgumentVector.push_back(Cast(ArgumentTypeAnnotation[called->getArg(index)], Arguments[index]->Render()));
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
	if (TypeAnnotation[callInstance].Size == 0) {
		SSA* c = Cast(DefinedTypes["byte"], callInstance);
		TypeAnnotation[c] = DefinedTypes["byte"];
		return c;
	}
	return callInstance;
}

SSA* FieldAccessExpression::Render() {
	EMIT_DEBUG;
	SSA* operand = Operand->Render();
	operand = DemotePointer(GetType(operand), operand);
	return IndexField(GetType(operand), Field, operand);
}

SSA* BreakExpression::Render() {
	EMIT_DEBUG;
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
	return ZeroSSA(DefinedTypes["dword"]);
}

SSA* ContinueExpression::Render() {
	EMIT_DEBUG;
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

	return ZeroSSA(DefinedTypes["dword"]);
}

SSA* BlockExpression::Render() {
	EMIT_DEBUG;
	if (Expressions.empty()) return ZeroSSA(DefinedTypes["dword"]);
	std::vector<SSA*> ExpressionVector;
	for (uint i = 0, e = Expressions.size(); i != e; i++) {
		ExpressionVector.push_back(Expressions[i]->Render());
		if (!ExpressionVector.back()) return nullptr;
	}

	return ExpressionVector.back();
}

SSA *ReturnExpression::Render() {
	EMIT_DEBUG;
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	SSA *returnValue;
	if (!ReturnValue) returnValue = llvm::Constant::getNullValue(function->getReturnType());
	else returnValue = Cast(ReturnTypeAnnotation[function], ReturnValue->Render());
	if (!returnValue) return nullptr;

	if (ReturnTypeAnnotation[function].UID == DefinedTypes["void"].UID) Builder->CreateRetVoid();
	else Builder->CreateRet(returnValue);
	RESOW_BASIC_BLOCK;
	return ZeroSSA(DefinedTypes["dword"]);
}

SSA* CVariadicArgumentExpression::Render() {
	EMIT_DEBUG;
	XLSVariable VAListStore = AllonymousValues["variadic"];
	SSA* VAList = Builder->CreateBitCast(VAListStore.Value, llvm::Type::getInt8PtrTy(*GlobalContext));
	SSA* R = Builder->CreateVAArg(VAList, Type.Type, "xls_variadic_get");
	TypeAnnotation[R] = Type;
	return R;
}

SSA* VariadicArgumentExpression::Render() {
	EMIT_DEBUG;
	XLSVariable HiveStore = AllonymousValues["#hive"];
	SSA* Hive = Builder->CreateLoad(HiveStore.Type.Type, HiveStore.Value, "xls_hive_value");
	SSA* bee = Builder->CreateBitCast(Hive, Type.Type->getPointerTo());
	SSA* R = Builder->CreateLoad(Type.Type, bee);
	SSA* GEP = Builder->CreateInBoundsGEP(Type.Type, bee, llvm::ConstantInt::get(DefinedTypes["dword"].Type, llvm::APInt(32, 1, false)));
	Builder->CreateStore(Builder->CreateBitCast(GEP, DefinedTypes["byte*"].Type), HiveStore.Value);
	TypeAnnotation[R] = Type;
	return R;
}

SSA* LabelExpression::Render() {
	EMIT_DEBUG;
	llvm::Function *function = Builder->GetInsertBlock()->getParent();
	SSA* Zero = ZeroSSA(DefinedTypes["dword"]);
	if (Name == CurrentLabelIdentifier && Mode == LABEL_DECLARE) {
		llvm::BasicBlock *NEW = llvm::BasicBlock::Create(*GlobalContext, "AL" + std::to_string(AnonymousLabels.size()), function);
		AnonymousLabels.push_back(NEW);
		return Zero;
	} else if (Name == CurrentLabelIdentifier && Mode == LABEL_DEFINE) {
		llvm::BasicBlock* label = AnonymousLabels.back();
		Builder->CreateBr(label);
		Builder->SetInsertPoint(label);
		if (Flags.Debug) {
			llvm::DILabel* dbgLbl = Dbg.Builder->createLabel(Dbg.Blocks.back(), "#AL" + std::to_string(AnonymousLabels.size()), Dbg.Blocks.back()->getFile(), GetRow());
			Dbg.Builder->insertLabel(dbgLbl, llvm::DILocation::get(Dbg.Blocks.back()->getContext(), GetRow(), GetColumn(), Dbg.Blocks.back()), Builder->GetInsertBlock());
		}
		return Zero;
	} else if (Name == "") {
		if (AnonymousReferer > AnonymousLabels.size()) {
			for(uint i = AnonymousLabels.size(); i <= AnonymousReferer; i++)
				AnonymousLabels.push_back(llvm::BasicBlock::Create(*GlobalContext, "AL" + std::to_string(i), function));
		}
		llvm::BasicBlock* label = AnonymousLabels[AnonymousReferer];
		Builder->CreateBr(label);
		Builder->SetInsertPoint(label);
		if (Flags.Debug) {
			llvm::DILabel* dbgLbl = Dbg.Builder->createLabel(Dbg.Blocks.back(), "#AL" + std::to_string(AnonymousLabels.size()), Dbg.Blocks.back()->getFile(), GetRow());
			Dbg.Builder->insertLabel(dbgLbl, llvm::DILocation::get(Dbg.Blocks.back()->getContext(), GetRow(), GetColumn(), Dbg.Blocks.back()), Builder->GetInsertBlock());
		}
		return Zero;
	}
	if (AllonymousLabels.find(Name) == AllonymousLabels.end()) AllonymousLabels[Name] = llvm::BasicBlock::Create(*GlobalContext, "L#" + Name, function);
	if (Mode == LABEL_DEFINE) {
		llvm::BasicBlock* label = AllonymousLabels[Name];
		Builder->CreateBr(label);
		Builder->SetInsertPoint(label);
		if (Flags.Debug) {
			llvm::DILabel* dbgLbl = Dbg.Builder->createLabel(Dbg.Blocks.back(), Name, Dbg.Blocks.back()->getFile(), GetRow());
			Dbg.Builder->insertLabel(dbgLbl, llvm::DILocation::get(Dbg.Blocks.back()->getContext(), GetRow(), GetColumn(), Dbg.Blocks.back()), Builder->GetInsertBlock());
		}
	}
	return Zero;
}

SSA* JumpExpression::Render() {
	EMIT_DEBUG;
	llvm::Function* function = Builder->GetInsertBlock()->getParent();
	SSA* Zero = ZeroSSA(DefinedTypes["dword"]);
	if (Label == CurrentLabelIdentifier) {
		llvm::BasicBlock* label = AnonymousLabels.back();
		Builder->CreateBr(label);
		RESOW_BASIC_BLOCK;
		return Zero;
	} else if (Label == "") {
		if (AnonymousReferer > AnonymousLabels.size()) {
			for (uint i = AnonymousLabels.size(); i <= AnonymousReferer; i++)
				AnonymousLabels.push_back(llvm::BasicBlock::Create(*GlobalContext, "AL" + std::to_string(i), function));
		}
		llvm::BasicBlock *label = AnonymousLabels[AnonymousReferer];
		Builder->CreateBr(label);
		RESOW_BASIC_BLOCK;
		return Zero;
	}
	// TODO: Jump to any expression that returns a label pointer.
	if (AllonymousValues.find(Label) != AllonymousValues.end()) {
		XLSVariable A = AllonymousValues[Label];
		llvm::LoadInst* V = Builder->CreateLoad(A.Type.Type, A.Value, "xls_jump&");
		llvm::IndirectBrInst* B = Builder->CreateIndirectBr(V, AllonymousLabels.size() + AnonymousLabels.size());
		for (std::pair<std::string, llvm::BasicBlock *> dest : AllonymousLabels) {
			B->addDestination(dest.second);
		}
		for (uint i = 0; i < AnonymousLabels.size(); i++) B->addDestination(AnonymousLabels[i]);
		RESOW_BASIC_BLOCK;
		return Zero;
	}
	if (AllonymousLabels.find(Label) == AllonymousLabels.end()) AllonymousLabels[Label] = llvm::BasicBlock::Create(*GlobalContext, "L#" + Label, function);
	llvm::BasicBlock* label = AllonymousLabels[Label];
	Builder->CreateBr(label);
	RESOW_BASIC_BLOCK;
	return Zero;
}

SSA* SetJumpExpression::Render() {
	EMIT_DEBUG;
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
	EMIT_DEBUG;
	llvm::Function* LongJumpIntrinsic = llvm::Intrinsic::getDeclaration(GlobalModule.get(), llvm::Intrinsic::eh_sjlj_longjmp);
	LongJumpIntrinsic->addFnAttr(llvm::Attribute::AlwaysInline);
	SSA* jumpBuffer = JumpBuffer->Render();
	std::vector<llvm::Type*> IType;
	std::vector<SSA*> IArg;
	IType.push_back(DefinedTypes["byte*"].Type);
	IArg.push_back(Builder->CreateBitCast(jumpBuffer, IType[0]));
	Builder->CreateCall(LongJumpIntrinsic, llvm::ArrayRef<SSA*>(IArg));
	return ZeroSSA(DefinedTypes["dword"]);
}

SSA* SizeofExpression::Render() {
	EMIT_DEBUG;
	SSA* value = Sized->Render();
	XLSType type = TypeAnnotation[value];
	SSA* R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, type.Size / 8, false));
	TypeAnnotation[R] = DefinedTypes["dword"];
	return R;
}

SSA* TypeofExpression::Render() {
	EMIT_DEBUG;
	SSA* value = Typed->Render();
	XLSType type = TypeAnnotation[value];
	SSA* R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, type.UID, false));
	TypeAnnotation[R] = DefinedTypes["dword"];
	return R;
}

SSA* CountofExpression::Render() {
	SSA* value = Counted->Render();
	XLSType type = GetType(value);
	if (type.IsRangedPointer) {
		SSA* R = Builder->CreateExtractValue(value, llvm::ArrayRef<unsigned>(RANGED_POINTER_COUNTOF), "xls_countof");
		TypeAnnotation[R] = DefinedTypes["#addrsize"];
		return R;
	}
	SSA* R = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, GetCountof(type), false));
	TypeAnnotation[R] = DefinedTypes["dword"];
	return R;
}

SSA* SetCountofExpression::Render() {
	EMIT_DEBUG;
	VariableExpression* rangedptrExpression = static_cast<VariableExpression *>(Counted.get());
	if (!rangedptrExpression) return CodeError("setcountof on fire.");
	XLSVariable rangedptr = FetchVirtualVariable(rangedptrExpression);
	if (!rangedptr.Type.IsRangedPointer) return CodeError("Cannot mutate the count of values that do not have ranged pointer type.");
	SSA* newCount = NewCount->Render();
	newCount = Cast(DefinedTypes["#addrsize"], newCount);
	ExdexRangedPointerCount(newCount, rangedptr);
	TypeAnnotation[newCount] = DefinedTypes["#addrsize"];
	return newCount;
}

SSA* MemsetExpression::Render() {
	EMIT_DEBUG;
	SSA* ptr = Ptr->Render();
	XLSType ptype = GetType(ptr);
	if (!ptype.IsPointer && !ptype.IsRangedPointer) return CodeError("Expected first argument to memset to be a ranged or unranged pointer.");
	if (Length == nullptr && !ptype.IsRangedPointer) return CodeError("Expected length argument or for the first argument of memset to be a ranged pointer.");
	SSA* length = (Length != nullptr) ? Cast(DefinedTypes["#addrsize"], Length->Render()) : Builder->CreateExtractValue(ptr, llvm::ArrayRef<unsigned>(RANGED_POINTER_COUNTOF));
	TypeAnnotation[length] = DefinedTypes["#addrsize"];
	SSA* value = Cast(DefinedTypes["byte"], Value->Render());
	// Since LLVM is incomprehensibly absurd, the inline memset intrinsic does not work for non-constant lengths‽ Thus, we roll our own.
	// We will have to instead use inline memset in small but efficient block sizes.
	SSA* pvalue = Cast(DefinedTypes["byte*"], ptr);
	llvm::Function* function = Builder->GetInsertBlock()->getParent();
	XLSVariable induction;
	induction.Global = false;
	induction.Name = ".#memset.tmp.bee";
	induction.Type = DefinedTypes["#addrsize"];
	induction.Value = createEntryBlockAlloca(function, "bee", induction.Type);
	AllonymousValues[".#memset.tmp.bee"] = induction;
	WriteVariable(ZeroSSA(DefinedTypes["#addrsize"]), induction);
	// OK, now we need the number of times the loop must run stored somewhere.
	SSA* n1 = llvm::ConstantInt::get(DefinedTypes["#addrsize"].Type, llvm::APInt(DefinedTypes["#addrsize"].Size, 1, false));
	SSA* n64 = llvm::ConstantInt::get(DefinedTypes["#addrsize"].Type, llvm::APInt(DefinedTypes["#addrsize"].Size, 64, false));
	SSA* modulus = Builder->CreateURem(length, n64, "memset-modulus");
	SSA* division = Builder->CreateUDiv(length, n64, "memset-division");
	// Time to make a loop!
	// Loop 1: Slow loop of the first modulus bytes.
	llvm::BasicBlock* conditionBlock = llvm::BasicBlock::Create(*GlobalContext, "memset-condition-slow", function);
	llvm::BasicBlock* loop = llvm::BasicBlock::Create(*GlobalContext, "memset-loop-slow", function);
	llvm::BasicBlock* after = llvm::BasicBlock::Create(*GlobalContext, "memset-after-slow", function);

	Builder->CreateBr(conditionBlock);
	Builder->SetInsertPoint(conditionBlock);
	SSA* condition = Builder->CreateICmpULT(ReadVariable(induction), modulus);
	Builder->CreateCondBr(condition, loop, after);

	Builder->SetInsertPoint(loop);
	SSA* GEP = Builder->CreateGEP(DefinedTypes["byte"].Type, pvalue, ReadVariable(induction), "memset-gep-slow");
	Builder->CreateStore(value, GEP);
	SSA* plusOne = Builder->CreateAdd(ReadVariable(induction), n1);
	TypeAnnotation[plusOne] = induction.Type;
	WriteVariable(plusOne, induction);

	Builder->CreateBr(conditionBlock);
	Builder->SetInsertPoint(after);

	pvalue = Builder->CreateGEP(DefinedTypes["byte"].Type, pvalue, ReadVariable(induction));
	WriteVariable(ZeroSSA(DefinedTypes["#addrsize"]), induction);
	// Loop 2: Fast loop of the rest of the bytes.
	conditionBlock = llvm::BasicBlock::Create(*GlobalContext, "memset-condition-fast", function);
	loop = llvm::BasicBlock::Create(*GlobalContext, "memset-loop-fast", function);
	after = llvm::BasicBlock::Create(*GlobalContext, "memset-after-fast", function);

	Builder->CreateBr(conditionBlock);
	Builder->SetInsertPoint(conditionBlock);
	condition = Builder->CreateICmpULT(ReadVariable(induction), division);
	Builder->CreateCondBr(condition, loop, after);

	Builder->SetInsertPoint(loop);
	SSA* shifted = Builder->CreateShl(ReadVariable(induction), llvm::ConstantInt::get(DefinedTypes["#addrsize"].Type, llvm::APInt(DefinedTypes["#addrsize"].Size, 6, false)));
	GEP = Builder->CreateGEP(DefinedTypes["byte"].Type, pvalue, shifted, "memset-gep-fast");
	Builder->CreateMemSetInline(GEP, llvm::MaybeAlign(), value, n64);
	plusOne = Builder->CreateAdd(ReadVariable(induction), n1);
	TypeAnnotation[plusOne] = induction.Type;
	WriteVariable(plusOne, induction);

	Builder->CreateBr(conditionBlock);
	Builder->SetInsertPoint(after);
	AllonymousValues.erase(".#memset.tmp.bee");

	return length;
}

SSA* MemcopyExpression::Render() {
	EMIT_DEBUG;
	SSA* destination = Destination->Render();
	SSA* source = Source->Render();
	XLSType dtype = GetType(destination);
	XLSType stype = GetType(source);
	if (!dtype.IsPointer && !dtype.IsRangedPointer) return CodeError("Expected first argument to memcopy to be a pointer or ranged pointer.");
	if (!stype.IsPointer && !stype.IsRangedPointer) return CodeError("Expected second argument to memcopy to be a pointer or ranged pointer.");
	if (Length == nullptr && !stype.IsRangedPointer) return CodeError("Expected length argument or for the second argument of memcopy to be a ranged pointer.");
	SSA* length = (Length != nullptr) ? Cast(DefinedTypes["#addrsize"], Length->Render()) : Builder->CreateExtractValue(source, RANGED_POINTER_COUNTOF);
	destination = Cast(DefinedTypes["byte*"], destination);
	source = Cast(DefinedTypes["byte*"], source);
	llvm::Function* function = Builder->GetInsertBlock()->getParent();
	XLSVariable induction;
	induction.Global = false;
	induction.Name = ".#memcopy.tmp.bee";
	induction.Type = DefinedTypes["#addrsize"];
	induction.Value = createEntryBlockAlloca(function, induction.Name, induction.Type);
	AllonymousValues[induction.Name] = induction;
	WriteVariable(ZeroSSA(induction.Type), induction);
	SSA* n1 = llvm::ConstantInt::get(DefinedTypes["#addrsize"].Type, llvm::APInt(DefinedTypes["#addrsize"].Size, 1, false));
	SSA* n64 = llvm::ConstantInt::get(DefinedTypes["#addrsize"].Type, llvm::APInt(DefinedTypes["#addrsize"].Size, 64, false));
	SSA* modulus = Builder->CreateURem(length, n64, "memcopy-modulus");
	SSA* division = Builder->CreateUDiv(length, n64, "memcopy-division");
	// Loop 1
	llvm::BasicBlock* conditionBlock = llvm::BasicBlock::Create(*GlobalContext, "memcopy-condition-slow", function);
	llvm::BasicBlock* loop = llvm::BasicBlock::Create(*GlobalContext, "memcopy-loop-slow", function);
	llvm::BasicBlock* after = llvm::BasicBlock::Create(*GlobalContext, "memcopy-after-slow", function);

	Builder->CreateBr(conditionBlock);
	Builder->SetInsertPoint(conditionBlock);
	SSA* i = ReadVariable(induction);
	SSA* condition = Builder->CreateICmpULT(i, modulus);
	Builder->CreateCondBr(condition, loop, after);

	Builder->SetInsertPoint(loop);
	SSA* GEPDestination = Builder->CreateGEP(DefinedTypes["byte"].Type, destination, i);
	SSA* GEPSource = Builder->CreateGEP(DefinedTypes["byte"].Type, source, i);
	Builder->CreateStore(Builder->CreateLoad(DefinedTypes["byte"].Type, GEPSource), GEPDestination);
	SSA* plusOne = Builder->CreateAdd(i, n1);
	TypeAnnotation[plusOne] = induction.Type;
	WriteVariable(plusOne, induction);

	Builder->CreateBr(conditionBlock);
	Builder->SetInsertPoint(after);

	destination = Builder->CreateGEP(DefinedTypes["byte"].Type, destination, i);
	source = Builder->CreateGEP(DefinedTypes["byte"].Type, source, i);
	WriteVariable(ZeroSSA(induction.Type), induction);

	// Loop 2
	conditionBlock = llvm::BasicBlock::Create(*GlobalContext, "memcopy-condition-fast", function);
	loop = llvm::BasicBlock::Create(*GlobalContext, "memcopy-loop-fast", function);
	after = llvm::BasicBlock::Create(*GlobalContext, "memcopy-after-fast", function);

	Builder->CreateBr(conditionBlock);
	Builder->SetInsertPoint(conditionBlock);
	i = ReadVariable(induction);
	condition = Builder->CreateICmpULT(i, division);
	Builder->CreateCondBr(condition, loop, after);

	Builder->SetInsertPoint(loop);
	SSA* shifted = Builder->CreateShl(ReadVariable(induction), llvm::ConstantInt::get(DefinedTypes["#addrsize"].Type, llvm::APInt(DefinedTypes["#addrsize"].Size, 6, false)));
	GEPDestination = Builder->CreateGEP(DefinedTypes["byte"].Type, destination, shifted);
	GEPSource = Builder->CreateGEP(DefinedTypes["byte"].Type, source, shifted);
	if (!RegionsOverlap) Builder->CreateMemCpyInline(GEPDestination, llvm::MaybeAlign(), GEPSource, llvm::MaybeAlign(), n64);
	else Builder->CreateMemMove(GEPDestination, llvm::MaybeAlign(), GEPSource, llvm::MaybeAlign(), n64);
	plusOne = Builder->CreateAdd(i, n1);
	TypeAnnotation[plusOne] = induction.Type;
	WriteVariable(plusOne, induction);

	Builder->CreateBr(conditionBlock);
	Builder->SetInsertPoint(after);
	AllonymousValues.erase(".#memcopy.tmp.bee");

	return length;
}

SSA* IfExpression::Render() {
	EMIT_DEBUG;
	SSA* condition = Condition->Render();
	if (!condition) return nullptr;

	if (condition->getType() != llvm::Type::getInt1Ty(*GlobalContext))
		condition = Builder->CreateICmpNE(condition, llvm::Constant::getNullValue(condition->getType()), "xls_if_condition");
	llvm::Function* function = Builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_then", function);
	llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_else");
	llvm::BasicBlock* afterBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_after_if");
	Builder->CreateCondBr(condition, thenBlock, elseBlock);

	Builder->SetInsertPoint(thenBlock);
	SSA* thenBranch = ThenBranch->Render();
	if (!thenBranch) return nullptr;

	Builder->CreateBr(afterBlock);
	thenBlock = Builder->GetInsertBlock();

	function->getBasicBlockList().push_back(elseBlock);
	Builder->SetInsertPoint(elseBlock);

	SSA* elseBranch = ElseBranch->Render();
	if (!elseBranch) return nullptr;

	Builder->CreateBr(afterBlock);
	elseBlock = Builder->GetInsertBlock();

	function->getBasicBlockList().push_back(afterBlock);
	Builder->SetInsertPoint(afterBlock);

	llvm::PHINode* phiNode = Builder->CreatePHI(GetType(thenBranch).Type, 2, "xls_if_block");
	phiNode->addIncoming(thenBranch, thenBlock);
	phiNode->addIncoming(Cast(GetType(thenBranch), elseBranch), elseBlock);
	TypeAnnotation[phiNode] = GetType(thenBranch);
	return phiNode;
}

SSA* CreateLogicalAnd(SSA* LHS, UQP(Expression) RHS, bool orMode) {
	if (LHS->getType() != llvm::Type::getInt1Ty(*GlobalContext))
		LHS = Builder->CreateICmpNE(LHS, ZeroSSA(GetType(LHS)), "xls_land_lhs_truth");

	llvm::Function* function = Builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* computeRHS = llvm::BasicBlock::Create(*GlobalContext, "xls_land_lhs_true", function);
	llvm::BasicBlock* noComputeRHS = llvm::BasicBlock::Create(*GlobalContext, "xls_land_lhs_false");
	llvm::BasicBlock* afterBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_after_land");
	if (!orMode) Builder->CreateCondBr(LHS, computeRHS, noComputeRHS);
	else Builder->CreateCondBr(LHS, noComputeRHS, computeRHS);

	Builder->SetInsertPoint(computeRHS);
	SSA* computedRHS = RHS->Render();
	if (!computedRHS) return nullptr;

	if (computedRHS->getType() != llvm::Type::getInt1Ty(*GlobalContext))
		computedRHS = Builder->CreateICmpNE(computedRHS, ZeroSSA(GetType(computedRHS)), "xls_land_rhs_truth");

	Builder->CreateBr(afterBlock);
	computeRHS = Builder->GetInsertBlock();

	function->getBasicBlockList().push_back(noComputeRHS);
	Builder->SetInsertPoint(noComputeRHS);

	SSA* terminalValue;
	if (!orMode) terminalValue = llvm::ConstantInt::getFalse(llvm::Type::getInt1Ty(*GlobalContext));
	else terminalValue = llvm::ConstantInt::getTrue(llvm::Type::getInt1Ty(*GlobalContext));
	Builder->CreateBr(afterBlock);
	noComputeRHS = Builder->GetInsertBlock();

	function->getBasicBlockList().push_back(afterBlock);
	Builder->SetInsertPoint(afterBlock);

	llvm::PHINode* phiNode = Builder->CreatePHI(llvm::Type::getInt1Ty(*GlobalContext), 2, "xls_land_result");
	phiNode->addIncoming(computedRHS, computeRHS);
	phiNode->addIncoming(terminalValue, noComputeRHS);
	TypeAnnotation[phiNode] = DefinedTypes["boole"];
	return phiNode;
}

SSA* WhileExpression::Render() {
	llvm::Function* function = Builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* conditionBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_while_check", function);
	llvm::BasicBlock* loopBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_loop", function);
	llvm::BasicBlock* afterBlock = llvm::BasicBlock::Create(*GlobalContext, "xls_after_loop", function);
	BreakStack.push(afterBlock);
	ContinueStack.push(conditionBlock);


	if (!DoWhile)Builder->CreateBr(conditionBlock);
	else Builder->CreateBr(loopBlock);

	Builder->SetInsertPoint(conditionBlock);
	SSA* condition = Condition->Render();
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

	return ZeroSSA(DefinedTypes["dword"]);
}

SSA* DeclarationExpression::Render() {
	EMIT_DEBUG;
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

		if (Flags.Debug) {
			llvm::DILocalVariable* dbgVar = Dbg.Builder->createAutoVariable(Dbg.Blocks.back(), variableName, Dbg.Blocks.back()->getFile(), GetRow(), Dbg.GetType(Type));
			Dbg.Builder->insertDeclare(alloca, dbgVar, Dbg.Builder->createExpression(), llvm::DILocation::get(Dbg.Blocks.back()->getContext(), GetRow(), GetColumn(), Dbg.Blocks.back()), Builder->GetInsertBlock());
		}

		Builder->CreateStore(Cast(Type, definerSSA), alloca);

		XLSVariable stored;
		stored.Type = Type;
		stored.Value = alloca;
		stored.Name = variableName;
		AllonymousValues[variableName] = stored;
	}

	return ZeroSSA(Type);
}

SSA* MacroExpression::Render() {
	EMIT_DEBUG;
	std::vector<XLiSp::Symbolic> Symbols;
	Symbols.push_back(XLiSp::SymbolicAtom(Name, true));
	XLiSp::SymbolicAtom k;
	for (int i = 0; i < Values.size(); i++) {
		switch (Metatypes[i].GetType()) {
		case MACRO_ARGUMENT_INTEGER:
			Symbols.push_back(XLiSp::SymbolicAtom(static_cast<DwordExpression*>(Values[i].get())->GetValue()));
			break;
		case MACRO_ARGUMENT_STRING:
			Symbols.push_back(XLiSp::SymbolicAtom(static_cast<StringExpression*>(Values[i].get())->GetValue(), false));
			break;
		case MACRO_ARGUMENT_EXPRESSION:
			Symbols.push_back(XLiSp::SymbolicAtom(Values[i]->Render()));
			break;
		case MACRO_ARGUMENT_TYPENAME:
		case MACRO_ARGUMENT_IDENTIFIER:
			k = XLiSp::SymbolicAtom(static_cast<VariableExpression*>(Values[i].get())->GetName(), true);
			k.Quoted = true;
			Symbols.push_back(k);
			break;
		default: return CodeError("Unknown render implementation for metatype.");
		}
	}

	auto WrapStream = std::queue<TokenContext>();
	TokenContext startend;
	startend.Value.Type = LEXEME_CHARACTER;
	startend.Value.Value = '{';
	WrapStream.push(startend);
	auto OutputStream = XLiSp::Symbolic(XLiSp::SymbolicList(Symbols)).Interpret(XLiSp::GlobalEnvironment).Tokenise(XLiSp::GlobalEnvironment);
	while (!OutputStream.empty()) {
		WrapStream.push(OutputStream.front());
		OutputStream.pop();
	}
	startend.Value.Value = '}';
	WrapStream.push(startend);
	TokenStream = WrapStream;

	GetNextToken();
	return ParseBlock()->Render();
}

SSA* XLiSpExpression::Render() {
	EMIT_DEBUG;
	return XLiSp::Evaluate(Stream)->Render();
}

SSA* ZeroSSA(XLSType Type) {
	SSA* R = llvm::Constant::getNullValue(Type.Type);
	TypeAnnotation[R] = Type;
	return R;
}

SSA* StructDefinition::Render() {
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

SSA* GlobalVariableNode::Render() {
	llvm::GlobalVariable* global = new llvm::GlobalVariable(*GlobalModule, Type.Type, false, llvm::GlobalValue::ExternalLinkage, 0, Name);
	global->setInitializer(llvm::Constant::getNullValue(Type.Type));
	XLSVariable variable;
	variable.Type = Type;
	variable.Value = global;
	variable.Name = Name;
	variable.Global = true;
	AllonymousValues[Name] = variable;
	TypeAnnotation[global] = Type;
	return global;
}

llvm::Function* SignatureNode::Render() {
	std::vector<llvm::Type*> ArgumentType;
	for (uint i = 0; i < Arguments.size(); i++)
		ArgumentType.push_back(Arguments[i].second.Type);

	if (Variadic == VARIADIC_XLS) {
		Arguments.push_back(SDX(std::string, XLSType)("#hive", DefinedTypes["byte*"]));
		ArgumentType.push_back(DefinedTypes["byte*"].Type);
	}

	llvm::FunctionType *functionType = llvm::FunctionType::get(Type.Type, ArgumentType, Variadic == VARIADIC_C);

	llvm::Function* function = llvm::Function::Create(functionType, Internal ? llvm::Function::PrivateLinkage : llvm::Function::ExternalLinkage, Name, GlobalModule.get());
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

llvm::Function* FunctionNode::Render() {
	SignatureNode &signature = *Signature;
	FunctionSignatures[Signature->GetName()] = std::move(Signature);
	llvm::Function* function = getFunction(signature.GetName());
	if (!function) return nullptr;

	if (signature.Binary()) BinaryPrecedence[signature.GetOperatorName()] = signature.GetOperatorPrecedence();
	if (signature.Unary()) BinaryPrecedence[signature.GetOperatorName()] = PRECEDENCE_UNPRECEDENTED;

	llvm::BasicBlock *basicBlock = llvm::BasicBlock::Create(*GlobalContext, "entry", function);
	Builder->SetInsertPoint(basicBlock);

	llvm::DIFile* dbgUnit;
	llvm::DIScope* dbgCtx;
	llvm::DISubprogram* dbgSP;

	if (Flags.Debug) {
		auto file = signature.GetFile();
		dbgUnit = Dbg.Builder->createFile(file.first == "" ? Dbg.Filename : file.first, file.first == "" ? Dbg.Directory : file.second);
		dbgCtx = dbgUnit;
		dbgSP = Dbg.Builder->createFunction(dbgCtx, signature.GetName(), llvm::StringRef(), dbgUnit, signature.GetRow(), Dbg.GetFunctionType(signature), signature.GetRow(), llvm::DINode::FlagPrototyped, llvm::DISubprogram::SPFlagDefinition);
		function->setSubprogram(dbgSP);
		Dbg.Blocks.push_back(dbgSP);
		Dbg.EmitLocation(Builder.get());
	}

	// Clear only local variables.
	auto iterator = AllonymousValues.begin();
	while (iterator != AllonymousValues.end()) {
		if (!iterator->second.Global) iterator = AllonymousValues.erase(iterator);
		else iterator++;
	}
	AllonymousLabels.clear();
	AnonymousLabels.clear();
	TypeAnnotation.clear();
	uint index = 0;
	for (llvm::Argument &argument : function->args()) {
		Alloca *alloca = createEntryBlockAlloca(function, argument.getName(), ArgumentTypeAnnotation[function->getArg(index)]);

		if (Flags.Debug) {
			llvm::DILocalVariable* dbgVar = Dbg.Builder->createParameterVariable(dbgSP, argument.getName(), index + 1, dbgUnit, signature.GetRow(), Dbg.GetType(ArgumentTypeAnnotation[function->getArg(index)]),true);
			Dbg.Builder->insertDeclare(alloca, dbgVar, Dbg.Builder->createExpression(), llvm::DILocation::get(dbgSP->getContext(), signature.GetRow(), 0, dbgSP), Builder->GetInsertBlock());
		}

		Builder->CreateStore(&argument, alloca);
		XLSVariable stored;
		stored.Type = ArgumentTypeAnnotation[function->getArg(index++)];
		stored.Value = alloca;
		AllonymousValues[std::string(argument.getName())] = stored;
	}

	if (Flags.Debug) Dbg.EmitLocation(Builder.get(), Body.get());

	if (function->isVarArg()) {
		Alloca* VAList = createEntryBlockAlloca(function, "variadic", DefinedTypes["valist"]);
		XLSVariable VAListStore;
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

	if (SSA* returnValue = Body->Render()) {
		if (signature.GetType().Name != "void") Builder->CreateRet(Cast(signature.GetType(), returnValue));
		else Builder->CreateRetVoid();
		if (Flags.Debug) Dbg.Blocks.pop_back();
		llvm::verifyFunction(*function);
		GlobalFPM->run(*function);
		return function;
	}

	function->eraseFromParent();
	if (Flags.Debug) Dbg.Blocks.pop_back();
	return nullptr;
}

SSA* NullNode::Render() {
	return CodeError("Rendering a null node is invalid.");
}

void InitialiseModule(std::string moduleName) {
	GlobalContext = MUQ(llvm::LLVMContext);
	GlobalModule = MUQ(llvm::Module, moduleName, *GlobalContext);
	GlobalFPM = MUQ(llvm::legacy::FunctionPassManager, GlobalModule.get());

	if (Flags.Debug) {
		Dbg.Builder = MUQ(llvm::DIBuilder, *GlobalModule.get());
		Dbg.CompileUnit = Dbg.Builder->createCompileUnit(llvm::dwarf::DW_LANG_C, Dbg.Builder->createFile("<stdin>", "."), "XLS Protocompiler", !Flags.NoOptimise, "<todo: fill in later>", 0, "output.dbg");
	}

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

	XLiSp::GlobalEnvironment = new XLiSp::Environment();

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

	XLSType JumpBufType;
	JumpBufType.Size = 5 * GlobalLayout->getPointerSizeInBits();
	JumpBufType.Type = llvm::Type::getIntNTy(*GlobalContext, JumpBufType.Size);
	JumpBufType.Name = "jumpbuf";
	JumpBufType.UID = CurrentUID++;

	DefinedTypes[JumpBufType.Name] = JumpBufType;
	TypeMap[JumpBufType.Type] = JumpBufType;

	// Base ranged pointer type
	std::vector<llvm::Type*> RPFields;
	RPFields.push_back(llvm::Type::getInt8PtrTy(*GlobalContext));
	RPFields.push_back(InternalAddressSizeType.Type);
	llvm::StructType* RPType = llvm::StructType::create(*GlobalContext, llvm::ArrayRef<llvm::Type*>(RPFields), "rangeptr");
	dword RPSize = GlobalLayout->getTypeSizeInBits(RPType);
	XLSType RangedBytePtrType {
		.Size = RPSize,
		.Type = RPType,
		.Name = "byte%",
		.IsRangedPointer = true,
		.Dereference = "byte",
		.UID = CurrentUID++
	};

	DefinedTypes[RangedBytePtrType.Name] = RangedBytePtrType;
	TypeMap[RPType] = RangedBytePtrType;

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
	(void)CheckTypeDefined(CurrentIdentifier);
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

void HandleFuncdef() {
	if (!ParseFuncdef()) GetNextToken();
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
		NEWType.IsStruct = DefinedTypes[dereference].IsStruct;
		NEWType.Structure = DefinedTypes[dereference].Structure;
		NEWType.UID = CurrentUID++;
		DefinedTypes[name] = NEWType;
		TypeMap[NEWType.Type] = NEWType;
		return true;
	}

	if (name.back() == '%') {
		dereference.pop_back();
		if (!CheckTypeDefined(dereference)) return false;
		XLSType NEWType {
			.Size = DefinedTypes["byte%"].Size,
			.Type = DefinedTypes["byte%"].Type,
			.Name = name,
			.IsRangedPointer = true,
			.IsStruct = DefinedTypes[dereference].IsStruct,
			.Signed = false,
			.Dereference = dereference,
			.Structure = DefinedTypes[dereference].Structure,
			.UID = CurrentUID++
		};

		DefinedTypes[name] = NEWType;
		return true;
	}

	if (name == "fn&") {
		std::string typeName;
		std::vector<XLSType> constituentTypes;
		typeName = "fn&(";
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
	FPType.Name = "fn&(";
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
