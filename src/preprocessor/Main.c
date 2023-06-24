#include "../num.def.h"
#include "Lexer.h"
#include "Preprocessor.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int main(int argc, char** argv) {
	NullUnit.Row = 0;
	NullUnit.File = NULL;
	PPUnit stdinUnit;
	stdinUnit.Row = 1;
	stdinUnit.File = stdin;
	puts("// preprocessed file");
	Preprocess(stdinUnit);
	return ferror(stdout);
}
