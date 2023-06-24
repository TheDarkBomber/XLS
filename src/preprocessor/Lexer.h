#ifndef __LEXER_XPP_H_
#define __LEXER_XPP_H_
#include "File.h"
#include <stdbool.h>

enum Directive {
	DIRECTIVE_INVALID = 0,
	DIRECTIVE_INCLUDE = 1,
	DIRECTIVE_DEFINE = 2
} typedef Directive;

Directive LexDirective(PPUnit stream);
PPUnit LexFilename(PPUnit stream);
char* LexDefine(PPUnit stream);
bool IsIdentifier(char c);

extern PPUnit NullUnit;

#endif
