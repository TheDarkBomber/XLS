#include "Lexer.hxx"
#include "Debug.hxx"
#include "num.def.h"
#include "macros.def.h"
#include "colours.def.h"
#include <ctype.h>
#include <stdio.h>
#include <string>
#include <iostream>

std::string CurrentOperator;
std::string CurrentIdentifier;
std::string LError;
dword CurrentInteger;

SourceLocation CurrentLocation;

char CharacterLiteral;
std::string StringLiteral;
StringTermination StringTerminator;

dword SpacesDetected = 0;
static bool Newline = false;

void* ExtraData;

char advance() {
	char next = getchar();
	if (Newline) {
		if (next == ' ') SpacesDetected++;
		if (next == '#') {
			InterpretDirective();
			next = '\v';
		}
		Newline = false;
	}
	CurrentLocation.File = Dbg.ExtraFiles.empty() ? std::make_pair("", "") : Dbg.ExtraFiles.back();
	if (next == '\n') {
		CurrentLocation.Row++;
		CurrentLocation.Column = 0;
		Newline = true;
	} else CurrentLocation.Column++;
	return next;
}

Token LexicalError(const char* error) {
	Token output;
	output.Type = LEXEME_END_OF_FILE;
	output.Subtype = LEXEME_END_OF_FILE;
	output.Value = '\0';
	LError = "";
	std::cerr <<
		COLOUR_YELLOW <<
		"R" << CurrentLocation.Row << "C" << CurrentLocation.Column << ": " <<
		COLOUR_PURPLE <<
		"Warning: " << COLOUR_PURPLE_BOLD << error <<
		COLOUR_END << '\n';
	return output;
}

Token GetToken() {
	static Token output;
	output.Value = '\0';
	output.Subtype = LEXEME_CHARACTER;
	static char LastCharacter = ' ';
	CurrentOperator = "";
	while (isspace(LastCharacter)) LastCharacter = advance();

	if (isalpha(LastCharacter) || LastCharacter == '_') {
		CurrentIdentifier = LastCharacter;
		while (IsIdentifier((LastCharacter = advance()))) {
			CurrentIdentifier += LastCharacter;
		}

		if (CMP("R", CurrentIdentifier) && LastCharacter == '"') {
			LastCharacter = LexString(LastCharacter);
			if (LError != "") return LexicalError(LError.c_str());
			LastCharacter = advance();
			StringTerminator = ST_RAW;
			output.Type = LEXEME_STRING;
			return output;
		}

		JMPIF(CurrentIdentifier, "implement", GetToken_kw_implement);
		JMPIF(CurrentIdentifier, "impl", GetToken_kw_implement);
		JMPIF(CurrentIdentifier, "extern", GetToken_kw_extern);
		JMPIF(CurrentIdentifier, "if", GetToken_kw_if);
		JMPIF(CurrentIdentifier, "else", GetToken_kw_else);
		JMPIF(CurrentIdentifier, "operator", GetToken_kw_operator);
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
		JMPIF(CurrentIdentifier, "mutable", GetToken_kw_mutable);
		JMPIF(CurrentIdentifier, "typeof", GetToken_kw_typeof);
		JMPIF(CurrentIdentifier, "struct", GetToken_kw_struct);
		JMPIF(CurrentIdentifier, "padded", GetToken_kw_structmode);
		JMPIF(CurrentIdentifier, "practical", GetToken_kw_structmode);
		JMPIF(CurrentIdentifier, "packed", GetToken_kw_structmode);
		JMPIF(CurrentIdentifier, "typedef", GetToken_kw_typedef);
		JMPIF(CurrentIdentifier, "break", GetToken_kw_break);
		JMPIF(CurrentIdentifier, "continue", GetToken_kw_continue);
		JMPIF(CurrentIdentifier, "return", GetToken_kw_return);
		JMPIF(CurrentIdentifier, "c-variadic", GetToken_kw_cvariadic);
		JMPIF(CurrentIdentifier, "variadic", GetToken_kw_variadic);
		JMPIF(CurrentIdentifier, "setjump", GetToken_kw_setjump);
		JMPIF(CurrentIdentifier, "longjump", GetToken_kw_longjump);
		JMPIF(CurrentIdentifier, "countof", GetToken_kw_countof);
		JMPIF(CurrentIdentifier, "setcountof", GetToken_kw_setcountof);
		JMPIF(CurrentIdentifier, "funcdef", GetToken_kw_funcdef);
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
	GetToken_kw_sizeof:
		output.Type = LEXEME_SIZEOF;
		return output;
	GetToken_kw_typeof:
		output.Type = LEXEME_TYPEOF;
		return output;
	GetToken_kw_setcountof:
		output.Subtype = LEXEME_ELSE;
	GetToken_kw_countof:
		output.Type = LEXEME_COUNTOF;
		return output;
	GetToken_kw_calling:
		output.Type = LEXEME_IDENTIFIER;
		output.Subtype = LEXEME_CALLING_CONVENTION;
		return output;
	GetToken_kw_structmode:
		output.Type = LEXEME_IDENTIFIER;
		output.Subtype = LEXEME_STRUCT_MODE;
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
	GetToken_kw_mutable:
		output.Type = LEXEME_MUTABLE;
		return output;
	GetToken_kw_volatile:
		output.Type = LEXEME_VOLATILE;
		return output;
	GetToken_kw_struct:
		output.Type = LEXEME_STRUCT;
		return output;
	GetToken_kw_typedef:
		output.Type = LEXEME_TYPEDEF;
		return output;
	GetToken_kw_break:
		output.Type = LEXEME_BREAK;
		return output;
	GetToken_kw_continue:
		output.Type = LEXEME_CONTINUE;
		return output;
	GetToken_kw_return:
		output.Type = LEXEME_RETURN;
		return output;
	GetToken_kw_cvariadic:
		output.Type = LEXEME_CVARIADIC;
		output.Subtype = LEXEME_VARIADIC;
		return output;
	GetToken_kw_variadic:
		output.Type = LEXEME_VARIADIC;
		output.Subtype = LEXEME_VARIADIC;
		return output;
	GetToken_kw_longjump:
		output.Subtype = LEXEME_ELSE;
	GetToken_kw_setjump:
		output.Type = LEXEME_SLJMP;
		return output;
	GetToken_kw_funcdef:
		output.Type = LEXEME_FUNCDEF;
		return output;
	GetToken_end_kw:
		output.Type = LEXEME_IDENTIFIER;
		return output;
	}

	if (isdigit(LastCharacter)) {
		std::string NumericalString;
		byte radix = 10;

		if (LastCharacter == '0') {
			char radixCharacter = advance();
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
			LastCharacter = advance();
		}

		while (IsRadix(LastCharacter, radix)) {
			NumericalString += LastCharacter;
			LastCharacter = advance();
		}

		CurrentInteger = strtoul(NumericalString.c_str(), 0, radix);
		output.Type = LEXEME_INTEGER;
		return output;
	}

	if (LastCharacter == '\'') {
		LastCharacter = advance();
		LastCharacter = LexCharacter(LastCharacter);
		if (LError != "") return LexicalError(LError.c_str());
		if ((LastCharacter = advance()) != '\'') return LexicalError("Expected closing apostrophe in character literal.");
		LastCharacter = advance();
		output.Type = LEXEME_CHARACTER_LITERAL;
		output.Value = CharacterLiteral;
		return output;
	}

	if (LastCharacter == '"') {
		LastCharacter = LexString(LastCharacter);
		if (LError != "") return LexicalError(LError.c_str());
		LastCharacter = advance();
		StringTerminator = ST_NULL;
		output.Type = LEXEME_STRING;
		return output;
	}

	if (LastCharacter == '/') {
		char nextCharacter = advance();
		if (nextCharacter == '/') {
			LastCharacter = nextCharacter;
			while (LastCharacter != EOF && LastCharacter != '\n' && LastCharacter != '\r' && LastCharacter != '\t')
				LastCharacter = advance();

			if (LastCharacter != EOF) return GetToken();
		}
	}

	if (IsOperator(LastCharacter)) {
		CurrentOperator = "";
		while (IsOperator(LastCharacter)) {
			CurrentOperator += LastCharacter;
			LastCharacter = advance();
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
	LastCharacter = advance();
	output.Type = LEXEME_CHARACTER;
	output.Value = current;
	return output;
}

char LexString(char LastCharacter) {
	StringLiteral = "";
	LastCharacter = advance();
	while (LastCharacter != '"') {
		LastCharacter = LexCharacter(LastCharacter);
		if (LError != "") return '\0';
		StringLiteral += CharacterLiteral;
		LastCharacter = advance();
	}
	return LastCharacter;
}

char LexCharacter(char LastCharacter) {
	if (LastCharacter == '\\') {
		LastCharacter = advance();
#define SYMEQ(symbol, character) case symbol: CharacterLiteral = character; break
		switch (LastCharacter) {
			SYMEQ('0', 0x00);
			SYMEQ('H', 0x01);
			SYMEQ('T', 0x02);
			SYMEQ('X', 0x03);
			SYMEQ('E', 0x04);
			SYMEQ('Q', 0x05);
			SYMEQ('A', 0x06);
			SYMEQ('a', '\a');
			SYMEQ('b', '\b');
		case 't': SYMEQ('h', '\t');
			SYMEQ('n', '\n');
			SYMEQ('v', '\v');
			SYMEQ('f', '\f');
			SYMEQ('r', '\r');
			SYMEQ('O', 0x0E);
			SYMEQ('I', 0x0F);
			SYMEQ('D', 0x10);
			SYMEQ('1', 0x11);
			SYMEQ('2', 0x12);
			SYMEQ('3', 0x13);
			SYMEQ('4', 0x14);
			SYMEQ('N', 0x15);
			SYMEQ('Y', 0x16);
			SYMEQ('B', 0x17);
			SYMEQ('C', 0x18);
			SYMEQ('M', 0x19);
			SYMEQ('S', 0x1A);
			SYMEQ('$', 0x1B);
			SYMEQ('F', 0x1C);
			SYMEQ('G', 0x1D);
			SYMEQ('R', 0x1E);
			SYMEQ('U', 0x1F);
			SYMEQ('*', 0x7F);
			SYMEQ('%', 0xFF);
		case 'x':
			char buffer[2];
			buffer[0] = LastCharacter = advance();
			buffer[1] = LastCharacter = advance();
			if (!IsRadix(buffer[0], 16) || !IsRadix(buffer[1], 16)) {
				LError = "Arbitrary value character literal malformed.";
				return '\0';
			}
			CharacterLiteral = (char)strtoul(std::string(buffer).c_str(), nullptr, 16);
			break;
		default:
			CharacterLiteral = LastCharacter;
			break;
		}
#undef SYMEQ
	} else CharacterLiteral = LastCharacter;
	return LastCharacter;
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
	case '$':
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
	case '%':
	case '&':
	case ':':
	case '@':
	case '=':
	case '>':
	case '<':
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
	fprintf(stderr, "String = %s\n", StringLiteral.c_str());
}

void InterpretDirective() {
	char type = getchar();
	if (type == 'F') {
		std::string file = "";
		char c;
		while ((c = getchar()) != '/') file += c;
		std::string nextLine = "";
		while ((c = getchar()) != '/') nextLine += c;
		std::string directory = "";
		while ((c = getchar()) != '\n') directory += c;
		CurrentLocation.Column = 0;
		CurrentLocation.Row = strtoul(nextLine.c_str(), 0, 10);
		Dbg.ExtraFiles.push_back(std::make_pair(file, directory));
	} else if (type == 'S') {
		std::string nextLine = "";
		char c;
		while ((c = getchar()) != '\n') nextLine += c;
		CurrentLocation.Column = 0;
		CurrentLocation.Row = strtoul(nextLine.c_str(), 0, 10);
		Dbg.ExtraFiles.pop_back();
	}
}
