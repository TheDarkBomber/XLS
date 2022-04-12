#ifndef __LEXER_XPP_H_
#define __LEXER_XPP_H_
#include <stdio.h>
#include <stdbool.h>

enum Directive {
	DIRECTIVE_INVALID = 0,
	DIRECTIVE_INCLUDE = 1,
	DIRECTIVE_DEFINE = 2
} typedef Directive;

Directive LexDirective(FILE* stream);
FILE* LexFilename(FILE* stream);
char* LexDefine(FILE* stream);
bool IsIdentifier(char c);

#endif
