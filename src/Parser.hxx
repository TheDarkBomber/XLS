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
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <string>
#include <vector>
#include <queue>
#include <memory>
#include <stack>
#include <map>

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

enum LabelMode {
	LABEL_DEFINE = 0,
	LABEL_DECLARE = 1
};

enum Variadism {
	VARIADIC_NONE = 0,
	VARIADIC_C = 1,
	VARIADIC_XLS = 2
};

enum MacroArgumentType {
	MACRO_ARGUMENT_INTEGER = 0,
	MACRO_ARGUMENT_STRING = 1,
	MACRO_ARGUMENT_EXPRESSION = 2,
	MACRO_ARGUMENT_TYPENAME = 3,
	MACRO_ARGUMENT_IDENTIFIER = 4,
	MACRO_ARGUMENT_VARIADIC = 5,
	MACRO_ARGUMENT_COMPOSITE = 6
};

struct ParserFlags {
	uint Unused : 2;
	uint Debug : 1;
	uint NoOptimise : 1;
	uint CodeWarning : 1;
	uint ParseWarning : 1;
	uint CodeError : 1;
	uint ParseError : 1;
} __attribute__((packed));

struct XLSType;
struct XLSFunctionInfo;

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
	bool IsRangedPointer = false;
	bool IsFP = false;
	bool IsStruct = false;
	bool IsLabel = false;
	bool Signed = false;
	std::string Dereference = "void";
	StructData Structure;
	std::vector<XLSType> FPData;
	dword UID;
};

struct XLSFunctionInfo {
	Variadism Variadic;
};

class Expression {
	SourceLocation DbgLoc;
public:
	Expression(SourceLocation dbgLoc = CurrentLocation) : DbgLoc(dbgLoc) {}
	virtual SSA *Render() = 0;
	dword GetRow() const { return DbgLoc.Row; }
	dword GetColumn() const { return DbgLoc.Column; }
	SDX(std::string, std::string) GetFile() const { return DbgLoc.File; }
};

class Statement {
public:
	virtual SSA *Render() = 0;
};

class DwordExpression : public Expression {
	dword Value;
public:
	DwordExpression(dword value) : Value(value) {}
	dword GetValue() { return Value; }
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
	std::string GetValue() { return Value; }
	SSA *Render() override;
};

class VariableExpression : public Expression {
	std::string Name;
	bool Volatile;
	bool Dereference;
	UQP(Expression) Offset;
	std::string Field;
	UQP(Expression) FieldOffset;
public:
	VariableExpression(const std::string &name, bool isVolatile = false, bool isDereference = false, UQP(Expression) offset = nullptr, std::string field = "", UQP(Expression) fieldOffset = nullptr) : Name(name), Volatile(isVolatile), Dereference(isDereference), Offset(std::move(offset)), Field(field), FieldOffset(std::move(fieldOffset)) {}
	SSA *Render() override;
	const std::string &GetName() const { return Name; }
	const bool &IsDereference() const { return Dereference; }
	const UQP(Expression) &GetOffset() const { return std::move(Offset); }
	const UQP(Expression) &GetFieldOffset() const { return std::move(FieldOffset); }
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
	UQP(Expression) DynamicSize;
	XLSType Type;
public:
	MutableArrayExpression(dword size, XLSType type, UQP(Expression) dynamicSize = nullptr) : Size(size), Type(type), DynamicSize(std::move(dynamicSize)) {}
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
	bool TailVariadic;
public:
	CallExpression(const std::string &called, std::vector<UQP(Expression)> arguments, bool tailVariadic = false) : Called(called), Arguments(std::move(arguments)), TailVariadic(tailVariadic) {}
	SSA *Render() override;
};

class FieldAccessExpression : public Expression {
	std::string Field;
	UQP(Expression) Operand;
public:
	FieldAccessExpression(const std::string &field, UQP(Expression) operand) : Field(field), Operand(std::move(operand)) {}
	SSA* Render() override;
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
	dword AnonymousReferer;
	LabelMode Mode;
public:
	LabelExpression(const std::string &name, dword aref, LabelMode mode) : Name(name), AnonymousReferer(aref), Mode(mode) {}
	SSA *Render() override;
};

class JumpExpression : public Expression {
	std::string Label;
	dword AnonymousReferer;
public:
	JumpExpression(const std::string &label, dword aref) : Label(label), AnonymousReferer(aref) {}
	SSA *Render() override;
};

class SetJumpExpression : public Expression {
	UQP(Expression) JumpBuffer;
public:
	SetJumpExpression(UQP(Expression) jumpBuffer) : JumpBuffer(std::move(jumpBuffer)) {}
	SSA* Render() override;
};

class LongJumpExpression : public Expression {
	UQP(Expression) JumpBuffer;
public:
	LongJumpExpression(UQP(Expression) jumpBuffer) : JumpBuffer(std::move(jumpBuffer)) {}
	SSA* Render() override;
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

class CountofExpression : public Expression {
	UQP(Expression) Counted;
public:
	CountofExpression(UQP(Expression) counted) : Counted(std::move(counted)) {}
	SSA* Render() override;
};

class SetCountofExpression : public Expression {
	UQP(Expression) Counted;
	UQP(Expression) NewCount;
public:
	SetCountofExpression(UQP(Expression) counted, UQP(Expression) newCount) : Counted(std::move(counted)), NewCount(std::move(newCount)) {}
	SSA* Render() override;
};

class MemsetExpression : public Expression {
	UQP(Expression) Ptr;
	UQP(Expression) Value;
	UQP(Expression) Length;
	bool Volatile;
public:
	MemsetExpression(UQP(Expression) ptr, UQP(Expression) value, bool isVolatile = false, UQP(Expression) length = nullptr) : Ptr(std::move(ptr)), Value(std::move(value)), Volatile(isVolatile), Length(std::move(length)) {}
	const bool HasLength() const { return Length != nullptr; }
	SSA* Render() override;
};

class MemcopyExpression : public Expression {
	UQP(Expression) Destination;
	UQP(Expression) Source;
	UQP(Expression) Length;
	bool Volatile;
	bool RegionsOverlap;
public:
	MemcopyExpression(UQP(Expression) destination, UQP(Expression) source, bool isVolatile = false, UQP(Expression) length = nullptr, bool overlap = false) : Destination(std::move(destination)), Source(std::move(source)), Volatile(isVolatile), Length(std::move(length)), RegionsOverlap(overlap) {}
	SSA* Render() override;
};

class CVariadicArgumentExpression : public Expression {
	XLSType Type;
public:
	CVariadicArgumentExpression(XLSType type) : Type(type) {}
	SSA *Render() override;
};

class VariadicArgumentExpression : public Expression {
	XLSType Type;
public:
	VariadicArgumentExpression(XLSType type) : Type(type) {}
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

class MacroArgument;
class MacroExpression : public Expression {
	std::string Name;
	std::vector<UQP(Expression)> Values;
	std::vector<MacroArgument> Metatypes;
public:
	MacroExpression(std::string name, std::vector<UQP(Expression)> values, std::vector<MacroArgument> metatypes) : Name(name), Values(std::move(values)), Metatypes(metatypes) {}
	SSA* Render() override;
};

class XLiSpExpression : public Expression {
	std::queue<TokenContext> Stream;
public:
	XLiSpExpression(std::queue<TokenContext> stream) : Stream(stream) {}
	SSA* Render() override;
};

class RawExpression : public Expression {
	SSA* Rendered;
public:
	RawExpression(SSA* rendered) : Rendered(rendered) {}
	SSA* Render() override { return Rendered; }
};

class SignatureNode {
	std::string Name;
	VDX(std::string, XLSType) Arguments;
	bool Operator;
	Precedence OperatorPrecedence;
	llvm::CallingConv::ID Convention;
	XLSType Type;
	Variadism Variadic;
	SourceLocation DbgLoc;
	bool Internal;
public:
	SignatureNode(const std::string &name, VDX(std::string, XLSType) arguments, const XLSType &type, SourceLocation dbgLoc = CurrentLocation, bool internal = false, Variadism variadic = VARIADIC_NONE, llvm::CallingConv::ID convention = llvm::CallingConv::C, bool operator_ = false, Precedence precedence = PRECEDENCE_INVALID) : Name(name), Arguments(std::move(arguments)), Type(type), DbgLoc(dbgLoc), Internal(internal), Variadic(variadic), Convention(convention), Operator(operator_), OperatorPrecedence(precedence) {}
	llvm::Function *Render();
	const std::string &GetName() const { return Name; }
	const XLSType &GetType() const { return Type; }
	const Variadism &GetVariadism() const { return Variadic; }
	const bool IsInternal() const { return Internal; }
	const VDX(std::string, XLSType) &GetArguments() const { return Arguments; }
	dword GetRow() const { return DbgLoc.Row; }
	dword GetColumn() const { return DbgLoc.Column; }
	SDX(std::string, std::string) GetFile() const { return DbgLoc.File; }
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

class MacroArgument {
	MacroArgumentType Type;
	std::vector<MacroArgument> Composite;
public:
	MacroArgument(const MacroArgumentType& type, std::vector<MacroArgument> composite, UQP(Expression) value) : Type(type), Composite(composite) {}
	MacroArgument(const MacroArgumentType& type) : Type(type) {}
	const MacroArgumentType& GetType() { return Type; }
	std::vector<MacroArgument> GetComposite() { return Composite; }
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

extern std::string CurrentIdentifier;
extern std::string CurrentOperator;
extern dword CurrentInteger;

extern dword CurrentRow;
extern dword CurrentColumn;

extern std::string StringLiteral;
extern StringTermination StringTerminator;

extern void* ExtraData;

Token GetNextToken();

UQP(Expression) ParseExpression(bool isVolatile = false);
UQP(Expression) ParseDwordExpression();
UQP(Expression) ParseCharacterExpression();
UQP(Expression) ParseStringExpression();
UQP(Expression) ParseParenthetical();
UQP(Expression) ParseIdentifier(bool isVolatile = false);
UQP(Expression) ParseDispatcher(bool isVolatile = false);
UQP(Expression) ParseIf();
UQP(Expression) ParseWhile(bool doWhile = false);
UQP(Expression) ParseUnary(bool isVolatile = false);
UQP(Expression) ParseDeclaration(XLSType type);
UQP(Expression) ParseMacro(std::string macro);
UQP(Expression) ParseBlock();
UQP(Expression) ParseLabel();
UQP(Expression) ParseJump();
UQP(Expression) ParseSetLongJump();
UQP(Expression) ParseSizeof();
UQP(Expression) ParseTypeof();
UQP(Expression) ParseCountof();
UQP(Expression) ParseSetCountof();
UQP(Expression) ParseMemset(bool isVolatile = false);
UQP(Expression) ParseMemcopy(bool isVolatile = false);
UQP(Expression) ParseMutable();
UQP(Expression) ParseBreak();
UQP(Expression) ParseContinue();
UQP(Expression) ParseCVariadic();
UQP(Expression) ParseVariadic();
UQP(Expression) ParseReturn();
UQP(Expression) ParsePostfix(UQP(Expression) LHS);

UQP(Expression) ParseBinary(Precedence precedence, UQP(Expression) LHS, bool isVolatile = false);

UQP(Expression) ParseXLiSp();
UQP(Expression) ParseRaw();

UQP(Statement) ParseStruct();
UQP(Statement) ParseGlobalVariable(XLSType type);
UQP(Statement) ParseTypedef();
UQP(Statement) ParseFuncdef();
UQP(Statement) ParseFuncdefMacro();

UQP(SignatureNode) ParseSignature();
UQP(SignatureNode) ParseExtern();

UQP(FunctionNode) ParseOperatorDefinition();
UQP(FunctionNode) ParseImplementation();
UQP(FunctionNode) ParseUnboundedExpression();

SSA* CreateLogicalAnd(SSA* LHS, UQP(Expression) RHS, bool orMode);

SSA* ZeroSSA(XLSType Type);
SSA* IntegerSSA(XLSType Type, dword Value);

Precedence GetTokenPrecedence();

void HandleOperatorDefinition();
void HandleImplementation();
void HandleExtern();
void HandleGlobal();
void HandleStruct();
void HandleTypedef();
void HandleFuncdef();
void HandleUnboundedExpression();

void PreinitialiseJIT();
void InitialiseModule(std::string moduleName);
void InitialiseJIT();

bool DefineFPType(std::string function, XLSType* outtype);

bool ConstructFPType();
bool ConstructArbIntType(bool sign = false);

bool CheckTypeDefined(std::string name);

SSA* CodeError(const char *error);

extern llvm::DataLayout* GlobalLayout;
extern llvm::Triple* GlobalTriple;

extern ParserFlags Flags;

extern dword CurrentUID;

extern std::map<std::string, Precedence> BinaryPrecedence;

extern UQP(llvm::LLVMContext) GlobalContext;
extern UQP(llvm::IRBuilder<>) Builder;
extern UQP(llvm::Module) GlobalModule;
extern UQP(llvm::legacy::FunctionPassManager) GlobalFPM;

extern std::map<std::string, UQP(SignatureNode)> FunctionSignatures;
extern std::map<std::string, llvm::BasicBlock*> AllonymousLabels;

extern std::map<llvm::Function*, XLSFunctionInfo> FunctionInfo;

extern std::vector<llvm::BasicBlock*> AnonymousLabels;
extern std::string CurrentLabelIdentifier;

extern std::stack<llvm::BasicBlock*> BreakStack;
extern std::stack<llvm::BasicBlock*> ContinueStack;

extern std::queue<TokenContext> TokenStream;

extern std::map<std::string, std::vector<MacroArgument>> Macros;

#endif
