#include "Lexer.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

PPUnit NullUnit;

Directive LexDirective(PPUnit stream) {
	char buffer[32];
	char c = fgetc(stream.File);
	for (unsigned i = 0; isalpha(c) && i < 32; i++, c = fgetc(stream.File)){
		buffer[i] = c;
	}
	if (strcmp(buffer, "include") == 0) return DIRECTIVE_INCLUDE;
	if (strcmp(buffer, "define") == 0) return DIRECTIVE_DEFINE;

	return DIRECTIVE_INVALID;
}

PPUnit LexFilename(PPUnit stream) {
	char buffer[256];
	if (fgets(buffer, 256, stream.File) == NULL) {
		return NullUnit;
	}
	char *n = strchr(buffer, '\n');
	if (n) *n = '\0';
	PPUnit unit;
	unit.File = fopen(buffer, "r");
	unit.Row = 1;
	char cwd[256];
	getcwd(cwd, 256);
	printf("#F%s/%d/%s\n", buffer, unit.Row, cwd);
	return unit;
}

char gbuffer[256];
char* LexDefine(PPUnit stream) {
	char buffer[256];
	if (fgets(buffer, 256, stream.File) == NULL) return NULL;
	char *n = strchr(buffer, '\n');
	if (n) *n = '\0';
	memcpy(gbuffer, buffer, 256);
	return gbuffer;
}

bool IsIdentifier(char c) {
	if (isalnum(c) || c == '_')
		return true;
	switch (c) {
	case '-':
	case '+':
	case '/':
	case '*':
	case ':':
	case '@':
	case '=':
	case '?':
	case '!':
		return true;
	default:
	  return false;
	}
}
