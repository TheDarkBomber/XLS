#include "Lexer.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>

Directive LexDirective(FILE* stream) {
	char buffer[32];
	char c = fgetc(stream);
	for (unsigned i = 0; isalpha(c) && i < 32; i++, c = fgetc(stream)){
		buffer[i] = c;
	}
	if (strcmp(buffer, "include") == 0) return DIRECTIVE_INCLUDE;

	return DIRECTIVE_INVALID;
}

FILE* LexFilename(FILE* stream) {
  char buffer[256];
  if (fgets(buffer, 256, stream) == NULL) {
    return NULL;
  }
  char *n = strchr(buffer, '\n');
  if (n) *n = '\0';
  return fopen(buffer, "r");
}
