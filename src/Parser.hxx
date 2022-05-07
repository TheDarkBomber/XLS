#ifndef __PARSER_XLS_H_
#define __PARSER_XLS_H_
#include "num.def.h"
#include "macros.def.h"
#include "Lexer.hxx"
#include <cassert>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <string>
#include <vector>
#include <memory>

#define RESOW_BASIC_BLOCK do { llvm::BasicBlock *resow = llvm::BasicBlock::Create(*GlobalContext, "xls_rs", Builder->GetInsertBlock()->getParent()); Builder->SetInsertPoint(resow); } while (0)

typedef llvm::Value SSA;
typedef llvm::AllocaInst Alloca;

enum Precedence {
	PRECEDENCE_INVALID = 0,
	PRECEDENCE_UNPRECEDENTED = 1,
	PRECEDENCE_ASSIGN = 2,
	PRECEDENCE_LOGICAL = 5,
	PRECEDENCE_BITWISE = 6,
	PRECEDENCE_COMPARE = 10,
	PRECEDENCE_ADD = 20,
	PRECEDENCE_USER_DEFAULT = 30,
	PRECEDENCE_MULTIPLY = 40,
	PRECEDENCE_PIPE = 50
};

enum SignatureType {
	SIGNATURE_NON_BINARY = 0,
	SIGNATURE_UNARY = 1,
	SIGNATURE_BINARY = 2
};

enum StructMode {
	STRUCT_PACKED = 0,
	STRUCT_PRACTICAL = 1,
	STRUCT_PADDED = 2
};

struct ParserFlags {
	uint Unused : 3;
	uint NoOptimise : 1;
	uint CodeWarning : 1;
	uint ParseWarning : 1;
	uint CodeError : 1;
	uint ParseError : 1;
} __attribute__((packed));

struct XLSType;

struct StructData {
	llvm::StructLayout* Layout = nullptr;
	bool Packed = false;
	dword LiteralSize;
	dword Size;
	std::map<std::string, SDX(dword, XLSType)> Fields;
};

struct XLSType {
  dword Size;
  llvm::Type *Type;
	std::string Name;
	bool IsPointer = false;
	bool IsFP = false;
	bool IsStruct = false;
	bool Signed = false;
	std::string Dereference = "void";
	StructData Structure;
	std::vector<XLSType> FPData;
	dword UID;
};

class Expression {
public:
	virtual SSA *Render() = 0;
};

class Statement {
public:
	virtual SSA *Render() = 0;
};

class DwordExpression : public Expression {
	dword Value;
public:
	DwordExpression(dword value) : Value(value) {}
	SSA *Render() override;
};

class CharacterExpression : public Expression {
	char Value;
public:
	CharacterExpression(char value) : Value(value) {}
	SSA *Render() override;
};

class StringExpression : public Expression {
	std::string Value;
	bool Mutable;
	StringTermination Terminator;
public:
	StringExpression(std::string value, StringTermination terminator = ST_NULL, bool _mutable = false) : Value(value), Terminator(terminator), Mutable(_mutable) {}
	SSA *Render() override;
};

class VariableExpression : public Expression {
	std::string Name;
	bool Volatile;
	bool Dereference;
	UQP(Expression) Offset;
	std::string Field;
public:
	VariableExpression(const std::string &name, bool isVolatile = false, bool isDereference = false, UQP(Expression) offset = nullptr, std::string field = "") : Name(name), Volatile(isVolatile), Dereference(isDereference), Offset(std::move(offset)), Field(field) {}
	SSA *Render() override;
	const std::string &GetName() const { return Name; }
	const bool &IsDereference() const { return Dereference; }
	const UQP(Expression) &GetOffset() const { return std::move(Offset); }
	const std::string &GetField() const { return Field; }
};

class DeclarationExpression : public Expression {
	VDX(std::string, UQP(Expression)) VariableNames;
	XLSType Type;
public:
	DeclarationExpression(VDX(std::string, UQP(Expression)) variableNames, XLSType type) : VariableNames(std::move(variableNames)), Type(type) {}
	const XLSType &GetType() const { return Type; }
	SSA *Render() override;
};

class MutableArrayExpression : public Expression {
	dword Size;
	XLSType Type;
public:
	MutableArrayExpression(dword size, XLSType type) : Size(size), Type(type) {}
	SSA *Render() override;
};

class CastExpression : public Expression {
	XLSType Type;
	UQP(Expression) Value;
public:
	CastExpression(XLSType type, UQP(Expression) value) : Type(type), Value(std::move(value)) {}
	SSA *Render() override;
};

class BinaryExpression : public Expression {
	std::string Operator;
	UQP(Expression) LHS, RHS;
	bool Volatile;
public:
	BinaryExpression(std::string operator_, UQP(Expression) LHS_, UQP(Expression) RHS_, bool isVolatile = false) : Operator(operator_), LHS(std::move(LHS_)), RHS(std::move(RHS_)), Volatile(isVolatile) {}
	SSA *Render() override;
};

class UnaryExpression : public Expression {
	std::string Operator;
	UQP(Expression) Operand;
public:
	UnaryExpression(std::string operator_, UQP(Expression) operand) : Operator(operator_), Operand(std::move(operand)) {}
	SSA *Render() override;
};

class CallExpression : public Expression {
	std::string Called;
	std::vector<UQP(Expression)> Arguments;
public:
	CallExpression(const std::string &called, std::vector<UQP(Expression)> arguments) : Called(called), Arguments(std::move(arguments)) {}
	SSA *Render() override;
};

class BreakExpression : public Expression {
  dword Nest;
public:
  BreakExpression(dword nest) : Nest(nest) {}
  SSA *Render() override;
};

class ContinueExpression : public Expression {
	dword Nest;
public:
	ContinueExpression(dword nest) : Nest(nest) {}
	SSA *Render() override;
};

class LabelExpression : public Expression {
	std::string Name;
public:
	LabelExpression(const std::string &name) : Name(name) {}
	SSA *Render() override;
};

class JumpExpression : public Expression {
	std::string Label;
public:
	JumpExpression(const std::string &label) : Label(label) {}
	SSA *Render() override;
};

class TypeofExpression : public Expression {
	UQP(Expression) Typed;
public:
	TypeofExpression(UQP(Expression) typed) : Typed(std::move(typed)) {}
	SSA *Render() override;
};

class SizeofExpression : public Expression {
	UQP(Expression) Sized;
public:
	SizeofExpression(UQP(Expression) sized) : Sized(std::move(sized)) {}
	SSA *Render() override;
};

class BlockExpression : public Expression {
	std::vector<UQP(Expression)> Expressions;
public:
	BlockExpression(std::vector<UQP(Expression)> expressions) : Expressions(std::move(expressions)) {}
	SSA *Render() override;
};

class ReturnExpression : public Expression {
	UQP(Expression) ReturnValue;
public:
	ReturnExpression(UQP(Expression) returnValue = nullptr) : ReturnValue(std::move(returnValue)) {}
	SSA *Render() override;
};

class IfExpression : public Expression {
  UQP(Expression) Condition, ThenBranch, ElseBranch;
public:
	IfExpression(UQP(Expression) condition, UQP(Expression) thenBranch, UQP(Expression) elseBranch) : Condition(std::move(condition)), ThenBranch(std::move(thenBranch)), ElseBranch(std::move(elseBranch)) {}
	SSA *Render() override;
};

class WhileExpression : public Expression {
	UQP(Expression) Condition, Body;
	bool DoWhile;
public:
	WhileExpression(UQP(Expression) condition, UQP(Expression) body, bool doWhile = false) : Condition(std::move(condition)), Body(std::move(body)), DoWhile(doWhile) {}
	SSA *Render() override;
};

class SignatureNode {
	std::string Name;
	VDX(std::string, XLSType) Arguments;
	bool Operator;
	Precedence OperatorPrecedence;
	llvm::CallingConv::ID Convention;
	XLSType Type;
public:
	SignatureNode(const std::string &name, VDX(std::string, XLSType) arguments, const XLSType &type, llvm::CallingConv::ID convention = llvm::CallingConv::C, bool operator_ = false, Precedence precedence = PRECEDENCE_INVALID) : Name(name), Arguments(std::move(arguments)), Type(type), Convention(convention), Operator(operator_), OperatorPrecedence(precedence) {}
	llvm::Function *Render();
	const std::string &GetName() const { return Name; }
	const XLSType &GetType() const { return Type; }
	bool Unary() const { return Operator && Arguments.size() == 1; }
	bool Binary() const { return Operator && Arguments.size() == 2; }
	std::string GetOperatorName() const {
		assert(Operator && "Assert that signature is signature for an operator.");
		// return Unary() ? Name.substr(13) : Name.substr(14);
		return Name.substr(12 + Arguments.size());
	}
	Precedence GetOperatorPrecedence() const { return OperatorPrecedence; }
};

class FunctionNode {
	UQP(SignatureNode) Signature;
	UQP(Expression) Body;
public:
	FunctionNode(UQP(SignatureNode) signature, UQP(Expression) body) : Signature(std::move(signature)), Body(std::move(body)) {}
	llvm::Function *Render();
};

class StructDefinition : public Statement {
	VDX(XLSType, std::string) Types;
  std::string Name;
  StructMode Mode;

public:
  StructDefinition(VDX(XLSType, std::string) types, std::string name, StructMode mode = STRUCT_PRACTICAL) : Types(types), Name(name), Mode(mode) {}
  SSA *Render() override;
};

class GlobalVariableNode : public Statement {
	std::string Name;
	dword Value;
	XLSType Type;
public:
	GlobalVariableNode(const std::string &name, XLSType type, dword value = 0) : Name(name), Type(type), Value(value) {}
	SSA *Render() override;
};

class NullNode : public Statement {
public:
	NullNode() {}
	SSA *Render() override;
};

struct AnnotatedValue {
	XLSType Type;
	Alloca* Value;
};

struct AnnotatedGlobal {
	XLSType Type;
	llvm::GlobalVariable* Value;
};

extern std::string CurrentIdentifier;
extern std::string CurrentOperator;
extern dword CurrentInteger;

extern dword CurrentRow;
extern dword CurrentColumn;

extern std::string StringLiteral;
extern StringTermination StringTerminator;

UQP(Expression) ParseExpression(bool isVolatile = false);
UQP(Expression) ParseDwordExpression();
UQP(Expression) ParseCharacterExpression();
UQP(Expression) ParseStringExpression();
UQP(Expression) ParseParenthetical();
UQP(Expression) ParseIdentifier(bool isVolatile = false);
UQP(Expression) ParseDispatcher();
UQP(Expression) ParseIf();
UQP(Expression) ParseWhile(bool doWhile = false);
UQP(Expression) ParseUnary();
UQP(Expression) ParseDeclaration(XLSType type);
UQP(Expression) ParseBlock();
UQP(Expression) ParseLabel();
UQP(Expression) ParseJump();
UQP(Expression) ParseSizeof();
UQP(Expression) ParseTypeof();
UQP(Expression) ParseMutable();
UQP(Expression) ParseBreak();
UQP(Expression) ParseContinue();
UQP(Expression) ParseReturn();

UQP(Expression) ParseBinary(Precedence precedence, UQP(Expression) LHS, bool isVolatile = false);

UQP(Statement) ParseStruct();
UQP(Statement) ParseGlobalVariable(XLSType type);
UQP(Statement) ParseTypedef();

UQP(SignatureNode) ParseSignature();
UQP(SignatureNode) ParseExtern();

UQP(FunctionNode) ParseOperatorDefinition();
UQP(FunctionNode) ParseImplementation();
UQP(FunctionNode) ParseUnboundedExpression();

XLSType GetType(SSA* toType);
SSA *ImplicitCast(XLSType type, SSA *toCast);

Precedence GetTokenPrecedence();

void HandleOperatorDefinition();
void HandleImplementation();
void HandleExtern();
void HandleGlobal();
void HandleStruct();
void HandleTypedef();
void HandleUnboundedExpression();

void PreinitialiseJIT();
void InitialiseModule(std::string moduleName);
void InitialiseJIT();

bool DefineFPType(std::string function, XLSType* outtype);
bool CheckTypeDefined(std::string name);

#endif
