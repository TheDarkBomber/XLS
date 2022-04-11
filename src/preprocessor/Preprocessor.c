#include "Preprocessor.h"
#include "Lexer.h"

void Preprocess(FILE *stream) {
	char c;
	while((c = fgetc(stream)) != EOF) {
		if (c == '\n') {
			putchar('\n');
			continue;
		}
		if (c == '#') {
			Directive d = LexDirective(stream);
			if (d == DIRECTIVE_INCLUDE) PreprocessInclude(stream);
		} else {
			putchar(c);
			while ((c = fgetc(stream)) != '\n') putchar(c);
			putchar('\n');
		}
	}
}

void PreprocessInclude(FILE *stream) {
	FILE *f = LexFilename(stream);
	if (!f) return;
	Preprocess(f);
	fclose(f);
}
