#ifndef __LEXER_XPP_H_
#define __LEXER_XPP_H_
#include <stdio.h>

enum Directive {
	DIRECTIVE_INVALID = 0,
	DIRECTIVE_INCLUDE = 1
} typedef Directive;

Directive LexDirective(FILE* stream);
FILE* LexFilename(FILE* stream);

#endif
