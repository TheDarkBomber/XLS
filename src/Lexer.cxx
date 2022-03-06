#include "Lexer.hxx"
#include "num.def.h"
#include "macros.def.h"
#include <ctype.h>
#include <stdio.h>
#include <string>

std::string CurrentIdentifier;
dword CurrentInteger;

Token GetToken() {
	static Token output;
	output.Value = '\0';
	static char LastCharacter = ' ';
	while (isspace(LastCharacter)) LastCharacter = getchar();

	if (isalpha(LastCharacter)) {
		CurrentIdentifier = LastCharacter;
		while (isalnum((LastCharacter = getchar()))) {
			CurrentIdentifier += LastCharacter;
		}

		JMPIF(CurrentIdentifier, "implement", GetToken_kw_implement);
		JMPIF(CurrentIdentifier, "impl", GetToken_kw_implement);
		JMPIF(CurrentIdentifier, "extern", GetToken_kw_extern);
		JMPIF(CurrentIdentifier, "if", GetToken_kw_if);
		JMPIF(CurrentIdentifier, "else", GetToken_kw_else);
		goto GetToken_end_kw;

	GetToken_kw_implement:
		output.Type = LEXEME_IMPLEMENT;
		return output;
	GetToken_kw_extern:
		output.Type = LEXEME_EXTERN;
		return output;
	GetToken_kw_if:
		output.Type = LEXEME_IF;
		return output;
	GetToken_kw_else:
		output.Type = LEXEME_ELSE;
		return output;
	GetToken_end_kw:
		output.Type = LEXEME_IDENTIFIER;
		return output;
	}

	if (isdigit(LastCharacter)) {
		std::string NumericalString;
		while (isdigit(LastCharacter)) {
			NumericalString += LastCharacter;
			LastCharacter = getchar();
		}

		CurrentInteger = strtoul(NumericalString.c_str(), 0, 10);
		output.Type = LEXEME_INTEGER;
		return output;
	}

	if (LastCharacter == '/') {
		char nextCharacter = getchar();
		if (nextCharacter == '/') {
			LastCharacter = nextCharacter;
			while (LastCharacter != EOF && LastCharacter != '\n' && LastCharacter != '\r')
				LastCharacter = getchar();

			if (LastCharacter != EOF) return GetToken();
		}
	}

	if (LastCharacter == EOF) {
		output.Type = LEXEME_END_OF_FILE;
		return output;
	}
	char current = LastCharacter;
	LastCharacter = getchar();
	output.Type = LEXEME_CHARACTER;
	output.Value = current;
	return output;
}
