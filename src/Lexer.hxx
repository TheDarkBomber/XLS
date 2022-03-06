#ifndef __LEXER_XLS_H_
#define __LEXER_XLS_H_

enum Lexeme {
	LEXEME_CHARACTER = 0,
	LEXEME_END_OF_FILE = 1,
	LEXEME_IMPLEMENT = 2,
	LEXEME_EXTERN = 3,
	LEXEME_IDENTIFIER = 4,
	LEXEME_INTEGER = 5
};

struct Token {
	Lexeme Type;
	char Value;
} __attribute__((packed));

Token GetToken();

#endif
