#ifndef __LEXER_XLS_H_
#define __LEXER_XLS_H_

#include "num.def.h"

enum Lexeme {
	LEXEME_CHARACTER = 0,
	LEXEME_END_OF_FILE = 1,
	LEXEME_IMPLEMENT = 2,
	LEXEME_EXTERN = 3,
	LEXEME_IDENTIFIER = 4,
	LEXEME_INTEGER = 5,
	LEXEME_IF = 6,
	LEXEME_ELSE = 7,
	LEXEME_OPERATOR = 8,
	LEXEME_DWORD_VARIABLE = 9,
	LEXEME_CALLING_CONVENTION = 10,
	LEXEME_WHILE = 11,
	LEXEME_LABEL = 12,
	LEXEME_JUMP = 13,
	LEXEME_VOLATILE = 14
};

struct Token {
	Lexeme Type;
	Lexeme Subtype;
	char Value;
} __attribute__((packed));

bool IsOperator(char c);
bool IsIdentifier(char c);
bool IsRadix(char c, byte radix);
void PrintToken(Token token);
Token GetToken();

#endif
