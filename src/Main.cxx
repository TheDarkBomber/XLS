#include "Lexer.hxx"
#include "Parser.hxx"
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
#include <stdio.h>

extern std::map<char, Precedence> BinaryPrecedence;
extern Token CurrentToken;
extern Token GetNextToken();

extern UQP(llvm::Module) GlobalModule;

static void REPL() {
	for(;;) {
		fprintf(stderr, "XLS: ");
		switch (CurrentToken.Type) {
		case LEXEME_END_OF_FILE:
			return;
		case LEXEME_IMPLEMENT:
			HandleImplementation();
			break;
		case LEXEME_EXTERN:
			HandleExtern();
			break;
		default:
			switch (CurrentToken.Value) {
			case ';':
				GetNextToken();
				break;
			default:
				HandleUnboundedExpression();
				break;
			}
		}
	}
}

int main(int argc, char* argv[]) {
	BinaryPrecedence['<'] = PRECEDENCE_COMPARE;
	BinaryPrecedence['>'] = PRECEDENCE_COMPARE;
	BinaryPrecedence['+'] = PRECEDENCE_ADD;
	BinaryPrecedence['-'] = PRECEDENCE_ADD;
	BinaryPrecedence['*'] = PRECEDENCE_MULTIPLY;

	PreinitialiseJIT();
	InitialiseJIT();
	fprintf(stderr, "XLS: ");
	GetNextToken();
	REPL();

	fprintf(stderr, "\n");
	GlobalModule->print(llvm::errs(), nullptr);

	return 0;
}
