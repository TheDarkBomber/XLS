#include "../num.def.h"
#include "Lexer.h"
#include "Preprocessor.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int main(int argc, char** argv) {
	Preprocess(stdin);
	return ferror(stdout);
}
