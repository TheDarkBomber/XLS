#ifndef __LEXER_XLS_H_
#define __LEXER_XLS_H_

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
	LEXEME_CALLING_CONVENTION = 10
};

struct Token {
	Lexeme Type;
	Lexeme Subtype;
	char Value;
} __attribute__((packed));

Token GetToken();

#endif
