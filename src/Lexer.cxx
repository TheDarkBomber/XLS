#include "Lexer.hxx"
#include "num.def.h"
#include "macros.def.h"
#include <ctype.h>
#include <stdio.h>
#include <string>

std::string CurrentOperator;
std::string CurrentIdentifier;
dword CurrentInteger;

Token GetToken() {
	static Token output;
	output.Value = '\0';
	output.Subtype = LEXEME_CHARACTER;
	static char LastCharacter = ' ';
	CurrentOperator = "";
	while (isspace(LastCharacter)) LastCharacter = getchar();

	if (isalpha(LastCharacter) || LastCharacter == '_') {
		CurrentIdentifier = LastCharacter;
		while (IsIdentifier((LastCharacter = getchar()))) {
			CurrentIdentifier += LastCharacter;
		}

		JMPIF(CurrentIdentifier, "implement", GetToken_kw_implement);
		JMPIF(CurrentIdentifier, "impl", GetToken_kw_implement);
		JMPIF(CurrentIdentifier, "extern", GetToken_kw_extern);
		JMPIF(CurrentIdentifier, "if", GetToken_kw_if);
		JMPIF(CurrentIdentifier, "else", GetToken_kw_else);
		JMPIF(CurrentIdentifier, "operator", GetToken_kw_operator);
		JMPIF(CurrentIdentifier, "dword", GetToken_kw_dword);
		JMPIF(CurrentIdentifier, "cdecl", GetToken_kw_calling);
		JMPIF(CurrentIdentifier, "fastcc", GetToken_kw_calling);
		JMPIF(CurrentIdentifier, "coldcc", GetToken_kw_calling);
		JMPIF(CurrentIdentifier, "tailcc", GetToken_kw_calling);
		JMPIF(CurrentIdentifier, "webkitjscc", GetToken_kw_calling);
		JMPIF(CurrentIdentifier, "win64cc", GetToken_kw_calling);
		JMPIF(CurrentIdentifier, "while", GetToken_kw_while);
		JMPIF(CurrentIdentifier, "dowhile", GetToken_kw_dowhile);
		JMPIF(CurrentIdentifier, "label", GetToken_kw_label);
		JMPIF(CurrentIdentifier, "jump", GetToken_kw_jump);
		JMPIF(CurrentIdentifier, "volatile", GetToken_kw_volatile);
		JMPIF(CurrentIdentifier, "sizeof", GetToken_kw_sizeof);
		JMPIF(CurrentIdentifier, "word", GetToken_kw_word);
		JMPIF(CurrentIdentifier, "byte", GetToken_kw_byte);
		JMPIF(CurrentIdentifier, "boole", GetToken_kw_boole);
		JMPIF(CurrentIdentifier, "void", GetToken_kw_void);
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
	GetToken_kw_operator:
		output.Type = LEXEME_OPERATOR;
		return output;
	GetToken_kw_dword:
		output.Type = LEXEME_DWORD_VARIABLE;
		output.Subtype = LEXEME_IDENTIFIER;
		return output;
	GetToken_kw_word:
		output.Type = LEXEME_WORD_VARIABLE;
		output.Subtype = LEXEME_IDENTIFIER;
		return output;
	GetToken_kw_byte:
		output.Type = LEXEME_BYTE_VARIABLE;
		output.Subtype = LEXEME_IDENTIFIER;
		return output;
	GetToken_kw_boole:
		output.Type = LEXEME_BOOLE_VARIABLE;
		output.Subtype = LEXEME_IDENTIFIER;
		return output;
	GetToken_kw_void:
		output.Type = LEXEME_VOID_VARIABLE;
		output.Subtype = LEXEME_IDENTIFIER;
		return output;
	GetToken_kw_sizeof:
		output.Type = LEXEME_SIZEOF;
		return output;
	GetToken_kw_calling:
		output.Type = LEXEME_IDENTIFIER;
		output.Subtype = LEXEME_CALLING_CONVENTION;
		return output;
	GetToken_kw_while:
		output.Type = LEXEME_WHILE;
		return output;
	GetToken_kw_dowhile:
		output.Type = LEXEME_WHILE;
		output.Subtype = LEXEME_ELSE;
		return output;
	GetToken_kw_label:
		output.Type = LEXEME_LABEL;
		return output;
	GetToken_kw_jump:
		output.Type = LEXEME_JUMP;
		return output;
	GetToken_kw_volatile:
		output.Type = LEXEME_VOLATILE;
		return output;
	GetToken_end_kw:
		output.Type = LEXEME_IDENTIFIER;
		return output;
	}

	if (isdigit(LastCharacter)) {
		std::string NumericalString;
		byte radix = 10;

		if (LastCharacter == '0') {
			char radixCharacter = getchar();
			LastCharacter = radixCharacter;
			switch (radixCharacter) {
			case 'd':
				break;
			case 'x':
				radix = 16;
				break;
			case 'b':
				radix = 2;
				break;
			case 'o':
				radix = 8;
				break;
			case 'q':
				radix = 4;
				break;
			case 's':
				radix = 6;
				break;
			case 'n':
				radix = 36;
				break;
			default:
				CurrentInteger = 0;
				output.Type = LEXEME_INTEGER;
				return output;
			}
			LastCharacter = getchar();
		}

		while (IsRadix(LastCharacter, radix)) {
			NumericalString += LastCharacter;
			LastCharacter = getchar();
		}

		CurrentInteger = strtoul(NumericalString.c_str(), 0, radix);
		output.Type = LEXEME_INTEGER;
		return output;
	}

	if (LastCharacter == '/') {
		char nextCharacter = getchar();
		if (nextCharacter == '/') {
			LastCharacter = nextCharacter;
			while (LastCharacter != EOF && LastCharacter != '\n' && LastCharacter != '\r' && LastCharacter != '\t')
				LastCharacter = getchar();

			if (LastCharacter != EOF) return GetToken();
		}
	}

	if (IsOperator(LastCharacter)) {
		CurrentOperator = "";
		while (IsOperator(LastCharacter)) {
			CurrentOperator += LastCharacter;
			LastCharacter = getchar();
		}
		output.Type = LEXEME_CHARACTER;
		output.Subtype = LEXEME_OPERATOR;
		output.Value = CurrentOperator.c_str()[0];
		return output;
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

bool IsOperator(char c) {
	if (isalpha(c) || isspace(c) || isdigit(c)) return false;
	switch (c) {
	case '#':
	case '\\':
	case '(':
	case ')':
	case '[':
	case ']':
	case '{':
	case '}':
	case ';':
	case '_':
	case EOF:
		return false;
	default: return true;
	}
}

bool IsIdentifier(char c) {
	if (isalnum(c) || c == '_') return true;
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
	default: return false;
	}
}

bool IsRadix(char c, byte radix) {
	char upper = toupper(c);
	if (radix < 2) return false;
	if (!isalnum(c)) return false;
	if (!isdigit(c) && radix <= 10) return false;
	if (upper > '9' && upper - radix > 'A') return false;
	if (upper <= '9' && upper - radix > '0') return false;
	return true;
}

void PrintToken(Token token) {
	fprintf(stderr, "TOKEN:\n");
	fprintf(stderr, "Type = %u\n", token.Type);
	fprintf(stderr, "Subtype = %u\n", token.Subtype);
	fprintf(stderr, "Value = %c\n", token.Value);
	fprintf(stderr, "CURRENT:\n");
	fprintf(stderr, "Operator = %s\n", CurrentOperator.c_str());
	fprintf(stderr, "Identifier = %s\n", CurrentIdentifier.c_str());
	fprintf(stderr, "Integer = %u\n", CurrentInteger);
	fprintf(stderr, "Alphanumeric = %s\n", isalnum(token.Value) ? "yes" : "no");
	fprintf(stderr, "Operator = %s\n", IsOperator(token.Value) ? "yes" : "no");
}
