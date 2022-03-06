#ifndef __PARSER_XLS_H_
#define __PARSER_XLS_H_
#include "num.def.h"
#include "macros.def.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Value.h>
#include <string>
#include <vector>
#include <memory>

typedef llvm::Value SSA;

enum Precedence {
	PRECEDENCE_INVALID = 0,
	PRECEDENCE_COMPARE = 10,
	PRECEDENCE_ADD = 20,
	PRECEDENCE_MULTIPLY = 40
};

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
};

class BinaryExpression : public Expression {
	char Operator;
	UQP(Expression) LHS, RHS;
public:
	BinaryExpression(char operator_, UQP(Expression) LHS_, UQP(Expression) RHS_) : Operator(operator_), LHS(std::move(LHS_)), RHS(std::move(RHS_)) {}
	SSA *Render() override;
};

class CallExpression : public Expression {
	std::string Called;
	std::vector<UQP(Expression)> Arguments;
public:
	CallExpression(const std::string &called, std::vector<UQP(Expression)> arguments) : Called(called), Arguments(std::move(arguments)) {}
	SSA *Render() override;
};

class IfExpression : public Expression {
  UQP(Expression) Condition, ThenBranch, ElseBranch;
public:
	IfExpression(UQP(Expression) condition, UQP(Expression) thenBranch, UQP(Expression) elseBranch) : Condition(std::move(condition)), ThenBranch(std::move(thenBranch)), ElseBranch(std::move(elseBranch)) {}
	SSA *Render() override;
};

class SignatureNode {
	std::string Name;
	std::vector<std::string> Arguments;
public:
	SignatureNode(const std::string &name, std::vector<std::string> arguments) : Name(name), Arguments(std::move(arguments)) {}
	llvm::Function *Render();
	const std::string &GetName() const { return Name; }
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

UQP(Expression) ParseBinary(Precedence precedence, UQP(Expression) LHS);

UQP(SignatureNode) ParseSignature();
UQP(SignatureNode) ParseExtern();

UQP(FunctionNode) ParseImplementation();
UQP(FunctionNode) ParseUnboundedExpression();

Precedence GetTokenPrecedence();

void HandleImplementation();
void HandleExtern();
void HandleUnboundedExpression();

void PreinitialiseJIT();
void InitialiseModule();
void InitialiseJIT();

#endif
