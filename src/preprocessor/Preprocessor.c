#include "../num.def.h"
#include "Preprocessor.h"
#include "Lexer.h"
#include "Map.h"
#include <stdlib.h>
#include <string.h>

void Preprocess(PPUnit stream) {
	char c;
	while((c = fgetc(stream.File)) != EOF) {
		if (c == '\n') {
			stream.Row++;
			putchar('\n');
			continue;
		}
		if (c == '#') {
			Directive d = LexDirective(stream);
			switch (d) {
			case DIRECTIVE_INCLUDE:
				PreprocessInclude(&stream);
				break;
			case DIRECTIVE_DEFINE:
				PreprocessDefine(stream);
				break;
			default: break;
			}
		} else {
			char ibuffer[256];
			char* v;
			do {
				memset(ibuffer, 0, 256);
				if (IsIdentifier(c)) {
					for (uint i = 0; IsIdentifier(c); i++, c = fgetc(stream.File)) {
						ibuffer[i] = c;
					}
					printf("%s", ((v = FindDefine(ibuffer)) == NULL) ? ibuffer : v);
					if (c != '\n') putchar(c);
					else break;
					continue;
				} else {
					putchar(c);
				}
			} while ((c = fgetc(stream.File)) != '\n'); // C dowhile has stupid syntax.
			putchar('\n');
		}
	}
}

void PreprocessInclude(PPUnit* stream) {
	PPUnit u = LexFilename(*stream);
	Preprocess(u);
	stream->Row++;
	printf("#S%d\n", stream->Row);
	fclose(u.File);
}

void PreprocessDefine(PPUnit stream) {
	char* define = LexDefine(stream);
	char* split = strchr(define, ' ');
	char* keybuffer = malloc(split - define + 1);
	char* vbuffer = malloc(strlen(split));
	*split = '\0';
	split++;
	memcpy(keybuffer, define, split - define + 1);
	memcpy(vbuffer, split, strlen(split));
	AddDefine(keybuffer, vbuffer);
}
