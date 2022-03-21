#ifndef __PARSER_XLS_H_
#define __PARSER_XLS_H_
#include "num.def.h"
#include "macros.def.h"
#include <cassert>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <string>
#include <vector>
#include <memory>

typedef llvm::Value SSA;
typedef llvm::AllocaInst Alloca;

enum Precedence {
	PRECEDENCE_INVALID = 0,
	PRECEDENCE_UNPRECEDENTED = 1,
	PRECEDENCE_ASSIGN = 2,
	PRECEDENCE_COMPARE = 10,
	PRECEDENCE_ADD = 20,
	PRECEDENCE_USER_DEFAULT = 30,
	PRECEDENCE_MULTIPLY = 40
};

enum SignatureType {
	SIGNATURE_NON_BINARY = 0,
	SIGNATURE_UNARY = 1,
	SIGNATURE_BINARY = 2
};

struct ParserFlags {
	uint Unused : 3;
	uint EmitIRToSTDOUT : 1;
	uint CodeWarning : 1;
	uint ParseWarning : 1;
	uint CodeError : 1;
	uint ParseError : 1;
} __attribute__((packed));

class Expression {
public:
	virtual SSA *Render() = 0;
};

class DwordExpression : public Expression {
	dword Value;
public:
	DwordExpression(dword value) : Value(value) {}
	SSA *Render() override;
};

class VariableExpression : public Expression {
	std::string Name;
public:
	VariableExpression(const std::string &name) : Name(name) {}
	SSA *Render() override;
	const std::string &GetName() const { return Name; }
};

class DwordDeclarationExpression : public Expression {
	VDX(std::string, UQP(Expression)) VariableNames;
public:
	DwordDeclarationExpression(VDX(std::string, UQP(Expression)) variableNames) : VariableNames(std::move(variableNames)) {}
	SSA *Render() override;
};

class BinaryExpression : public Expression {
	char Operator;
	UQP(Expression) LHS, RHS;
public:
	BinaryExpression(char operator_, UQP(Expression) LHS_, UQP(Expression) RHS_) : Operator(operator_), LHS(std::move(LHS_)), RHS(std::move(RHS_)) {}
	SSA *Render() override;
};

class UnaryExpression : public Expression {
	char Operator;
	UQP(Expression) Operand;
public:
	UnaryExpression(char operator_, UQP(Expression) operand) : Operator(operator_), Operand(std::move(operand)) {}
	SSA *Render() override;
};

class CallExpression : public Expression {
	std::string Called;
	std::vector<UQP(Expression)> Arguments;
public:
	CallExpression(const std::string &called, std::vector<UQP(Expression)> arguments) : Called(called), Arguments(std::move(arguments)) {}
	SSA *Render() override;
};

class BlockExpression : public Expression {
	std::vector<UQP(Expression)> Expressions;
public:
	BlockExpression(std::vector<UQP(Expression)> expressions) : Expressions(std::move(expressions)) {}
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
	std::vector<std::string> Arguments;
	bool Operator;
	Precedence OperatorPrecedence;
	llvm::CallingConv::ID Convention;
public:
	SignatureNode(const std::string &name, std::vector<std::string> arguments, llvm::CallingConv::ID convention = llvm::CallingConv::C, bool operator_ = false, Precedence precedence = PRECEDENCE_INVALID) : Name(name), Arguments(std::move(arguments)), Convention(convention), Operator(operator_), OperatorPrecedence(precedence) {}
	llvm::Function *Render();
	const std::string &GetName() const { return Name; }
	bool Unary() const { return Operator && Arguments.size() == 1; }
	bool Binary() const { return Operator && Arguments.size() == 2; }
	char GetOperatorName() const {
		assert(Operator && "Assert that signature is signature for an operator.");
		return Name[Name.size() - 1];
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

extern std::string CurrentIdentifier;
extern dword CurrentInteger;

UQP(Expression) ParseExpression();
UQP(Expression) ParseDwordExpression();
UQP(Expression) ParseParenthetical();
UQP(Expression) ParseIdentifier();
UQP(Expression) ParseDispatcher();
UQP(Expression) ParseIf();
UQP(Expression) ParseWhile(bool doWhile = false);
UQP(Expression) ParseUnary();
UQP(Expression) ParseDwordDeclaration();
UQP(Expression) ParseBlock();

UQP(Expression) ParseBinary(Precedence precedence, UQP(Expression) LHS);

UQP(SignatureNode) ParseSignature();
UQP(SignatureNode) ParseExtern();

UQP(FunctionNode) ParseOperatorDefinition();
UQP(FunctionNode) ParseImplementation();
UQP(FunctionNode) ParseUnboundedExpression();

Precedence GetTokenPrecedence();

void HandleOperatorDefinition();
void HandleImplementation();
void HandleExtern();
void HandleUnboundedExpression();

void PreinitialiseJIT();
void InitialiseModule(std::string moduleName);
void InitialiseJIT();

#endif
