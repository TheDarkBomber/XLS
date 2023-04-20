#include "XLiSp.hxx"
#include "Lexer.hxx"
#include "colours.def.h"
#include <stdarg.h>

namespace XLiSp {
	std::map<std::string, Fun> SymbolFunctionMap;
	static Environment* GlobalEnvironment = nullptr;

	static SymbolicAtom AddFun(SymbolicList symbolList, Environment* env) {
		dword total = 0;
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		for (int i = 1; i < symbols.size(); i++) {
			SymbolicAtom operand = symbols[i].Interpret(env);
			if (operand.Type != XLISP_INTEGER) return XLiSpError("Cannot add non-integer values.\n");
			total += operand.Integer;
		}
		return SymbolicAtom(total);
	}

	static SymbolicAtom MulFun(SymbolicList symbolList, Environment* env) {
		dword total = 1;
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		for (int i = 1; i < symbols.size(); i++) {
			SymbolicAtom operand = symbols[i].Interpret(env);
			if (operand.Type != XLISP_INTEGER) return XLiSpError("Cannot multiply non-integer values.\n");
			total *= operand.Integer;
		}
		return SymbolicAtom(total);
	}

	static SymbolicAtom ModulusFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		if (symbols.size() < 2) return XLiSpError("Expected at least 2 elements to i%, but got %lu.\n", symbols.size());
		SymbolicAtom modulator = symbols[1].Interpret(env);
		SymbolicAtom modulated = symbols[2].Interpret(env);
		if (modulator.Type != XLISP_INTEGER || modulated.Type != XLISP_INTEGER)
			return XLiSpError("Cannot modulate non-integer values.\n");
		if (modulated.Integer == 0) return XLiSpError("Cannot modulate by zero.\n");
		dword result = modulator.Integer % modulated.Integer;
		return SymbolicAtom(result);
	}

	static SymbolicAtom EqualsFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		if (symbols.size() < 2) return XLiSpError("Expected at least 2 elements to c=, but got %lu.\n", symbols.size());
		SymbolicAtom A = symbols[1].Interpret(env);
		SymbolicAtom B = symbols[2].Interpret(env);
		if (A.Type != B.Type) return SymbolicAtom(false);
		switch (A.Type) {
		case XLISP_INTEGER: return SymbolicAtom(A.Integer == B.Integer);
		case XLISP_STRING: return SymbolicAtom(A.String == B.String);
		default: return XLiSpError("Invalid types for equality comparison.\n");
		}
		return SymbolicAtom(false);
	}

	static SymbolicAtom SetFun(SymbolicList symbolList, Environment* env) {
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		if (symbols.size() < 3)
			return XLiSpError("Expected at least 3 elements to set!, but got %lu.\n", symbols.size());

		if (!symbols[1].IsAtomic()) return XLiSpError("Expected first argument to set! to be atomic.\n");
		SymbolicAtom atomKey = symbols[1].GetAtom();
		if (atomKey.Type != XLISP_IDENTIFIER) return XLiSpError("Expected first argument to set! to be an identifier.\n");
		env->Set(atomKey.String, Symbolic(symbols[2].Interpret(env)));
		return SymbolicAtom();
	}

	static SymbolicAtom LetFun(SymbolicList symbolList, Environment* env) {
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		if (symbols.size() < 2) return XLiSpError("Expected at least 2 elements to let*, but got %lu.\n", symbols.size());
		Environment* newEnv = new Environment(env);
		if (symbols[1].IsAtomic()) return XLiSpError("Expected first argument to let* to be non-atomic\n");
		std::vector<Symbolic> bindlist = symbols[1].GetList().GetSymbols();
		for (int i = 0; i < bindlist.size(); i += 2) {
			if (!bindlist[i].IsAtomic()) return XLiSpError("Expected atom in let* bindings\n");
			if (bindlist[i].GetAtom().Type != XLISP_IDENTIFIER) return XLiSpError("Let* binding list must be only identifiers.\n");
			newEnv->Set(bindlist[i].GetAtom().String, bindlist[i + 1].Interpret(newEnv));
		}
		SymbolicAtom result = symbols[2].Interpret(newEnv);
		if (result.Type == XLISP_CLOSURE) newEnv->ExheritClosure(result.Enclosure);
		delete newEnv;
		return result;
	}

	static SymbolicAtom DoFun(SymbolicList symbolList, Environment* env) {
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		SymbolicAtom last = SymbolicAtom();
		for (int i = 1; i < symbols.size(); i++) {
			last = symbols[i].Interpret(env);
		}
		return last;
	}

	static SymbolicAtom IfFun(SymbolicList symbolList, Environment* env) {
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		if (symbols.size() < 3) return XLiSpError("Expected at least 3 elements to if, but got %lu\n", symbols.size());
		SymbolicAtom condition = symbols[1].Interpret(env);
		if (condition.Truth) return symbols[2].Interpret(env);
		return symbols[3].Interpret(env);
	}

	static SymbolicAtom LambdaFun(SymbolicList symbolList, Environment* env) {
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		if (symbols[1].IsAtomic()) return XLiSpError("Argument list to lambda must be a list.\n");
		Closure* closure = new Closure(symbols[1].GetList(), symbols[2].Expand(env));
		env->AddClosure(closure);
		return SymbolicAtom(closure);
	}

	static SymbolicAtom EchoFun(SymbolicList symbolList, Environment* env) {
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		if (symbols.size() < 1) return XLiSpError("Expected at least 1 element to echo, but got %lu\n", symbols.size());
		SymbolicAtom operand = symbols[1].Interpret(env).CastToString();
		// TODO: Echo all arguments in sequence.
		printf("%s\n", operand.String.c_str());
		return operand;
	}

	static SymbolicAtom ErrorFun(SymbolicList symbolList, Environment* env) {
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		if (symbols.size() < 1) return XLiSpError("Expected at least 1 element to echo, but got %lu\n", symbols.size());
		SymbolicAtom errorMsg = symbols[1].Interpret(env);
		if (errorMsg.Type != XLISP_STRING) return XLiSpError("Expected string as input to echo.\n");
		return XLiSpError("%s\n", errorMsg.String.c_str());
	}

	static SymbolicAtom AsStringFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		if (symbols.size() < 1) return XLiSpError("Expected at least 1 element to as-string, but got %lu\n", symbols.size());
		SymbolicAtom atom = symbols[1].Interpret(env);
		return atom.CastToString();
	}

	static SymbolicAtom StrcatFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		std::string total = "";
		for (int i = 1; i < symbols.size(); i++) {
			SymbolicAtom atom = symbols[i].Interpret(env).CastToString();
			total += atom.String;
		}
		return SymbolicAtom(total, false);
	}

	static SymbolicAtom JoinTokensFun(SymbolicList symbolList, Environment* env) {
		auto symbols =  symbolList.GetSymbols();
		if (symbols.size() < 1) return XLiSpError("Expected at least 1 element to join-tokens, but got %lu\n", symbols.size());
		std::vector<TokenOrConsequences> tlist;
		TokenContext startendToken;
		startendToken.Value.Type = LEXEME_CHARACTER;
		startendToken.Value.Value = '{';
		tlist.push_back(startendToken);
		SymbolicAtom inputList = symbols[1].Interpret(env);
		if (inputList.Type != XLISP_LIST) return XLiSpError("Expected input to join-tokens to be a list.\n");
		auto symlist = inputList.List.GetSymbols();
		for (int i = 0; i < symlist.size(); i++) {
			SymbolicAtom atom = symlist[i].Interpret(env);
			if (atom.Type != XLISP_TOKEN_LIST) return XLiSpError("Expected token-list from argument %d of join-tokens\n", i);
			std::vector<TokenOrConsequences> atomList = atom.TokenList.GetList();
			for (int j = 0; j < atomList.size(); j++) tlist.push_back(atomList[j]);
			startendToken.Value.Value = ';';
			tlist.push_back(startendToken);
		}
		startendToken.Value.Value = '}';
		tlist.push_back(startendToken);
		return SymbolicAtom(ListOfTokens(tlist));
	}

	static SymbolicAtom ForFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		if (symbols.size() < 4) return XLiSpError("Expected at least 4 elements to for, but got %lu\n", symbols.size());
		if (!symbols[1].IsAtomic()) return XLiSpError("Expected first element to for to be atomic.\n");
		SymbolicAtom bindAtom = symbols[1].GetAtom();
		if (bindAtom.Type != XLISP_IDENTIFIER) return XLiSpError("Expected first element to for to be an identifier.\n");
		SymbolicAtom startAtom = symbols[2].Interpret(env);
		SymbolicAtom endAtom = symbols[3].Interpret(env);
		if (startAtom.Type != XLISP_INTEGER) return XLiSpError("Expected second element to for to be an integer.\n");
		if (endAtom.Type != XLISP_INTEGER) return XLiSpError("Expected third element to for to be an integer.\n");
		if (symbols[4].IsAtomic()) return XLiSpError("Expected fourth element to for to be a list.\n");
		SymbolicList actionList = symbols[4].GetList();
		std::vector<Symbolic> resultSymbols;
		for (dword i = startAtom.Integer; i <= endAtom.Integer; i++) {
			Environment* forEnv = new Environment(env);
			SymbolicAtom iAtom = SymbolicAtom(i);
			forEnv->Set(bindAtom.String, iAtom);
			SymbolicAtom result = actionList.Interpret(forEnv);
			if (result.Type == XLISP_CLOSURE) forEnv->ExheritClosure(result.Enclosure);
			delete forEnv;
			resultSymbols.push_back(result);
		}
		return SymbolicAtom(SymbolicList(resultSymbols));
	}

	static SymbolicAtom ListFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		std::vector<Symbolic> outlist;
		for (int i = 1; i < symbols.size(); i++) outlist.push_back(symbols[i].Interpret(env));
		return SymbolicAtom(SymbolicList(outlist));
	}

	static SymbolicAtom QuoteFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		SymbolicAtom atom = symbols[1].IsAtomic() ? symbols[1].GetAtom() : symbols[1].GetList();
		atom.Quoted =  true;
		return atom;
	}

	static SymbolicAtom UnquoteFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		SymbolicAtom atom = symbols[1].Interpret(env);
		if (!atom.Quoted) return atom;
		atom.Quoted = false;
		return Symbolic(atom).Interpret(env);
	}

	static SymbolicAtom QuasiquoteFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		if (symbols[1].IsAtomic()) {
			SymbolicAtom atom = symbols[1].GetAtom();
			atom.Quoted = true;
			return atom;
		}
		SymbolicList toQuasi = symbols[1].GetList().Quasiquote(env);
		if (toQuasi.GetSymbols().size() > 0 && (toQuasi.GetSymbols()[0].IsSymbol("unquote") || toQuasi.GetSymbols()[0].IsSymbol("splice-unquote"))) return toQuasi.Interpret(env);
		SymbolicAtom atom = toQuasi;
		atom.Quoted = true;
		return atom;
	}

	static SymbolicAtom MacroFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		if (symbols.size() < 3) return XLiSpError("Expected 2 elements to macro!, but got %lu\n", symbols.size() - 1);
		if (!symbols[1].IsAtomic()) return XLiSpError("Expected first element to macro! to be atomic.\n");
		SymbolicAtom name = symbols[1].GetAtom();
		if (name.Type != XLISP_IDENTIFIER) return XLiSpError("Expected first element to macro! to be an identifier.\n");
		SymbolicAtom closure = symbols[2].Interpret(env);
		if (closure.Type != XLISP_CLOSURE) return XLiSpError("Expected second element to macro! to be a closure.\n");
		closure.Enclosure->Macro = true;
		env->Set(name.String, closure);
		return SymbolicAtom();
	}

	static SymbolicAtom MacroExpandFun(SymbolicList symbolList, Environment* env) {
		auto symbols = symbolList.GetSymbols();
		Symbolic expanded = symbols[1].MacroExpand(env);
		std::vector<Symbolic> newSymbols;
		newSymbols.push_back(SymbolicAtom("quote", true));
		newSymbols.push_back(expanded);
		return QuoteFun(newSymbols, env);
	}

	UQP(Expression) Evaluate(std::queue<TokenContext> InputStream) {
		SymbolFunctionMap["i+"] = AddFun;
		SymbolFunctionMap["i*"] = MulFun;
		SymbolFunctionMap["i%"] = ModulusFun;
		SymbolFunctionMap["c="] = EqualsFun;
		SymbolFunctionMap["set!"] = SetFun;
		SymbolFunctionMap["let*"] = LetFun;
		SymbolFunctionMap["do"] = DoFun;
		SymbolFunctionMap["if"] = IfFun;
		SymbolFunctionMap["lambda"] = LambdaFun;
		SymbolFunctionMap["echo"] = EchoFun;
		SymbolFunctionMap["error"] = ErrorFun;
		SymbolFunctionMap["as-string"] = AsStringFun;
		SymbolFunctionMap["strcat"] = StrcatFun;
		SymbolFunctionMap["join-tokens"] = JoinTokensFun;
		SymbolFunctionMap["for"] = ForFun;
		SymbolFunctionMap["list"] = ListFun;
		SymbolFunctionMap["quote"] = QuoteFun;
		SymbolFunctionMap["unquote"] = UnquoteFun;
		SymbolFunctionMap["quasiquote"] = QuasiquoteFun;
		SymbolFunctionMap["splice-unquote"] = UnquoteFun;
		SymbolFunctionMap["macro!"] = MacroFun;
		SymbolFunctionMap["macroexpand"] = MacroExpandFun;
		if (!GlobalEnvironment) GlobalEnvironment = new Environment();
		UQP(SymbolicParser) parser = MUQ(SymbolicParser, InputStream);
		Symbolic symbol = Symbolic(parser->ParseList());
		TokenContext t;
		TokenStream = std::queue<TokenContext>();
		t.Value.Type = LEXEME_CHARACTER;
		t.Value.Value = '{';
		TokenStream.push(t);
		std::queue<TokenContext> OutputStream = symbol.Interpret(GlobalEnvironment).Tokenise(GlobalEnvironment);
		while (!OutputStream.empty()) {
			TokenStream.push(OutputStream.front());
			OutputStream.pop();
		}
		t.Value.Value = '}';
		TokenStream.push(t);
		GetNextToken();
		return ParseBlock();
	}

	Symbolic SymbolicParser::ParseSymbolic() {
		TokenContext t = Stream.front();
		if (t.Value.Type == LEXEME_CHARACTER) {
			switch(t.Value.Value) {
			case '(': // ')'
				Stream.pop();
				return Symbolic(this->ParseList());
			case '%':
				Stream.pop();
				return this->ParseReaderMacro("quote");
			case '~':
				Stream.pop();
				return this->ParseReaderMacro("unquote");
			case '`': // '`'
				Stream.pop();
				return this->ParseReaderMacro("quasiquote");
			case ',':
				Stream.pop();
				return this->ParseReaderMacro("splice-unquote");
			}
		}
		return Symbolic(this->ParseAtom());
	}

	SymbolicList SymbolicParser::ParseList() {
		if (Stream.empty()) return SymbolicList();
		std::vector<Symbolic> symbols;
		while (!Stream.empty() && Stream.front().Value.Value != ')') {
			symbols.push_back(this->ParseSymbolic());
			Stream.pop();
		}
		return SymbolicList(symbols);
	}

	SymbolicAtom SymbolicParser::ParseAtom() {
		TokenContext t = Stream.front();
		switch (t.Value.Type) {
		case LEXEME_STRING:
			return SymbolicAtom(t.StringLiteral, false);
		case LEXEME_IDENTIFIER:
			if (t.Identifier == "true") return SymbolicAtom(true);
			if (t.Identifier == "false") return SymbolicAtom(false);
			if (t.Identifier == "null") return SymbolicAtom();
			return SymbolicAtom(t.Identifier, true);
		case LEXEME_INTEGER:
			return SymbolicAtom(t.IntegerLiteral);
		case LEXEME_CHARACTER:
			switch (t.Value.Value) {
			case '[': // ']'
				return this->ParseTokenList();
			}
		case LEXEME_END_OF_FILE:
		case LEXEME_CHARACTER_LITERAL:
			return XLiSpError("Attempted to understand token %d as atom.\n", t.Value.Type);
		default:
			return SymbolicAtom(t.Identifier, true);
		}
	}

	Symbolic SymbolicParser::ParseReaderMacro(std::string special) {
		std::vector<Symbolic> outlist;
		outlist.push_back(SymbolicAtom(special, true));
		outlist.push_back(this->ParseSymbolic());
		return SymbolicList(outlist);
	}

	SymbolicAtom SymbolicParser::ParseTokenList() {
		Stream.pop();
		TokenContext t = Stream.front();
		std::vector<TokenOrConsequences> tokens = std::vector<TokenOrConsequences>();
		dword bnest = 0;
		while (t.Value.Type != LEXEME_CHARACTER || t.Value.Value != ']' || bnest > 0) {
			if (t.Value.Type == LEXEME_CHARACTER && t.Value.Value == '$') {
				Stream.pop();
				t = Stream.front();
				if (t.Value.Type != LEXEME_CHARACTER || t.Value.Value != '(')
					return XLiSpError("Expected open parenthesis after $ in token list.\n");
				dword nest = 0;
				std::queue<TokenContext> symbolicStream;
				Stream.pop(); t = Stream.front();
				while (t.Value.Value != ')' || nest > 0) {
					symbolicStream.push(t);
					if (t.Value.Value == '(') nest++;
					if (t.Value.Value == ')') nest--;
					Stream.pop(); t = Stream.front();
				}
				Stream.pop(); t = Stream.front();
				UQP(SymbolicParser) parser = MUQ(SymbolicParser, symbolicStream);
				Symbolic symbol = parser->ParseSymbolic();
				if (symbol.IsAtomic())
				tokens.push_back(TokenOrConsequences(parser->ParseList()));
			} else {
				if (t.Value.Type == LEXEME_CHARACTER && t.Value.Value == '[') bnest++;
				if (t.Value.Type == LEXEME_CHARACTER && t.Value.Value == ']') bnest--;
				tokens.push_back(TokenOrConsequences(t));
				Stream.pop();
				t = Stream.front();
			}
		}
		return SymbolicAtom(ListOfTokens(tokens));
	}

	SymbolicAtom Symbolic::Interpret(Environment* env) {
		*this = MacroExpand(env);
		if (Atomic) {
			if (Atom.Quoted) return Atom;
			if (Atom.Type == XLISP_IDENTIFIER && env->Find(Atom.String))
				return env->Get(Atom.String).Interpret(env);
			if (Atom.Type == XLISP_TOKEN_LIST)
				return Atom.TokenList.Interpret(env);
			if (Atom.Type == XLISP_LIST) return Atom.List.Interpret(env);
			return Atom;
		}
 		return List.Interpret(env);
	}

	SymbolicAtom SymbolicList::Interpret(Environment* env) {
		if (Symbols[0].IsAtomic()) {
			SymbolicAtom atom = Symbols[0].GetAtom();
			if (atom.Quoted) return atom;
			if (atom.Type == XLISP_IDENTIFIER && SymbolFunctionMap.find(atom.String) != SymbolFunctionMap.end()) {
				return SymbolFunctionMap[atom.String](*this, env);
			} else if (atom.Type == XLISP_IDENTIFIER && env->Find(atom.String)) {
				Symbolic envSymbol = env->Get(atom.String);
				Symbols[0] = envSymbol.Interpret(env);
				return this->Interpret(env);
			}

			if (atom.Type == XLISP_CLOSURE) return atom.Enclosure->Interpret(*this, env);
			if (atom.Type == XLISP_TOKEN_LIST) return atom.TokenList.Interpret(env);
			if (atom.Type == XLISP_LIST) return atom.List.Interpret(env);
			return SymbolicAtom(*this);
		}
		Symbols[0] = Symbols[0].Interpret(env);
		return this->Interpret(env);
	}

	SymbolicAtom Closure::Interpret(SymbolicList symbolList, Environment* env) {
		std::vector<Symbolic> symbols = symbolList.GetSymbols();
		Environment* newEnv = new Environment(env);
		std::vector<Symbolic> vars = Arguments.GetSymbols();
		for (int i = 0; i < vars.size(); i++) {
			if (!vars[i].IsAtomic()) return XLiSpError("Expected atomic argument in closure bindings list, got non-atomic argument instead.\n");
			if (vars[i].GetAtom().Type != XLISP_IDENTIFIER) return XLiSpError("Closure argument list must contain only identifiers.\n");
			std::string atomKey = vars[i].GetAtom().String;
			newEnv->Set(atomKey, symbols[i + 1].Interpret(env));
		}
		SymbolicAtom result = Body.Interpret(newEnv);
		if (result.Type == XLISP_CLOSURE) newEnv->ExheritClosure(result.Enclosure);
		delete newEnv;
		return result;
	}

	Symbolic Symbolic::Expand(Environment* env) {
		if (Atomic) {
			if (Atom.Type == XLISP_IDENTIFIER && env->Find(Atom.String))
				return env->Get(Atom.String);
			return Atom;
		}
		return List.Expand(env);
	}

	Symbolic SymbolicList::Expand(Environment* env) {
		for (int i = 0; i < Symbols.size(); i++) Symbols[i] = Symbols[i].Expand(env);
		return *this;
	}

	SymbolicList SymbolicList::Quasiquote(Environment* env) {
		std::vector<Symbolic> outsymbols;
		for (Symbolic S : Symbols) {
			if (S.IsAtomic()) outsymbols.push_back(S);
			else {
				std::vector<Symbolic> checkSymbols = S.GetList().GetSymbols();
				if (checkSymbols.size() > 1 && checkSymbols[0].IsSymbol("unquote"))
					outsymbols.push_back(checkSymbols[1].Interpret(env));
				else if (checkSymbols.size() > 0 && checkSymbols[0].IsSymbol("splice-unquote")) {
					SymbolicAtom atom = S.Interpret(env);
					if (atom.Type != XLISP_LIST) {outsymbols.push_back(atom); continue;}
					std::vector<Symbolic> toSplice = atom.List.GetSymbols();
					for (Symbolic K : toSplice) outsymbols.push_back(K);
				} else outsymbols.push_back(S.GetList().Quasiquote(env));
			}
		}
		return outsymbols;
	}

	std::queue<TokenContext> SymbolicList::Tokenise(Environment* env) {
		std::queue<TokenContext> outstream;
		TokenContext startend;
		startend.Value.Type = LEXEME_CHARACTER;
		startend.Value.Value = '{';
		outstream.push(startend);
		for (Symbolic symbol : Symbols) {
			std::queue<TokenContext> Q = symbol.Interpret(env).Tokenise(env);
			while (!Q.empty()) {
				TokenContext t = Q.front();
				outstream.push(t);
				Q.pop();
				t.Value.Type = LEXEME_CHARACTER;
				t.Value.Value = ';';
				outstream.push(t);
			}
		}
		startend.Value.Value = '}';
		outstream.push(startend);
		return outstream;
	}

	std::queue<TokenContext> SymbolicAtom::Tokenise(Environment* env) {
		std::queue<TokenContext> outstream;
		TokenContext t;
		switch (Type) {
		case XLISP_NULL: break;
		case XLISP_INTEGER:
			t.Value.Type = LEXEME_INTEGER;
			t.IntegerLiteral = Integer;
			outstream.push(t);
			break;
		case XLISP_STRING:
			t.Value.Type = LEXEME_STRING;
			t.StringLiteral = String;
			outstream.push(t);
			break;
		case XLISP_IDENTIFIER:
			t.Value.Type = LEXEME_IDENTIFIER;
			t.Identifier = String;
			outstream.push(t);
			break;
		case XLISP_LIST:
			return List.Tokenise(env);
		case XLISP_BOOLE:
			t.Value.Type = LEXEME_INTEGER;
			t.IntegerLiteral = (dword)Truth;
			outstream.push(t);
			break;
		case XLISP_CLOSURE:
			t.Value.Type = LEXEME_STRING;
			t.StringLiteral = "(XLiSp closure)";
			outstream.push(t);
			break;
		case XLISP_TOKEN_LIST:
			return TokenList.Tokenise(env);
		default: exit(3054);
		}
		return outstream;
	}

	std::queue<TokenContext> TokenOrConsequences::Interpret(Environment* env) {
		std::queue<TokenContext> R;
		if (!IsConsequences) {
			R.push(Token);
			return R;
		}

		SymbolicAtom A = Consequences.Interpret(env);
		return A.Tokenise(env);
	}

	Symbolic Symbolic::MacroExpand(Environment* env) {
		Symbolic retsymbolic = *this;
		while (retsymbolic.IsMacro(env)) {
			SymbolicList list = Atomic ? Atom.List : List;
			std::vector<Symbolic> listSymbols = list.GetSymbols();
			Closure* closure = env->Get(listSymbols[0].Atom.String).Atom.Enclosure;
			for (int i = 1; i < listSymbols.size(); i++) {
				SymbolicAtom atom = listSymbols[i].IsAtomic() ? listSymbols[i].GetAtom() : listSymbols[i].GetList();
				atom.Quoted = true;
				listSymbols[i] = atom;
			}
			list = listSymbols;
			SymbolicAtom result = closure->Interpret(list, env);
			result.Quoted = false;
			retsymbolic = result;
			retsymbolic = retsymbolic.Dequote();
		}
		return retsymbolic;
	}

	Symbolic Symbolic::Dequote() {
		SymbolicAtom atom = Atomic ? Atom : List;
		atom.Quoted = false;
		if (atom.Type == XLISP_LIST) {
			std::vector<Symbolic> symbols = atom.List.GetSymbols();
			for (int i = 0; i < symbols.size(); i++) symbols[i] = symbols[i].Dequote();
			return SymbolicList(symbols);
		}
		return atom;
	}

	bool Symbolic::IsMacro(Environment* env) {
		SymbolicList list;
		if (Atomic && Atom.Type != XLISP_LIST) return false;
		list = Atomic ? Atom.List : List;
		std::vector<Symbolic> symbols = list.GetSymbols();
		if (symbols.size() < 1) return false;
		if (!symbols[0].IsAtomic()) return false;
		SymbolicAtom atom = symbols[0].Atom;
		if (atom.Type != XLISP_IDENTIFIER) return false;
		if (!env->Find(atom.String)) return false;
		Symbolic S = env->Get(atom.String);
		if (!S.IsAtomic() || !S.GetAtom().Enclosure->Macro) return false;
		return true;
	}

	bool Symbolic::IsSymbol(std::string symbol) {
		return this->IsAtomic() && this->GetAtom().Type == XLISP_IDENTIFIER && this->GetAtom().String == symbol;
	}

	void Environment::Set(std::string key, Symbolic value) {
		SymbolMap[key] = value;
	}

	Environment* Environment::Find(std::string key) {
		if (SymbolMap.find(key) == SymbolMap.end()) {
			if (Parent) return Parent->Find(key);
			return nullptr;
		}
		return this;
	}

	Symbolic Environment::Get(std::string key) {
		Environment* SearchLocation = Find(key);
		if (!SearchLocation) exit(7077);
		return SearchLocation->GetSymbolMap()[key];
	}

	void Environment::AddChild(Environment* child) {
		Children.push_back(child);
	}

	void Environment::AddClosure(Closure* closure) {
		Closures.push_back(closure);
	}

	void Environment::ExheritClosure(Closure* closure) {
		auto iterator = Closures.begin();
		while (iterator != Closures.end()) {
			if (*iterator == closure) {
				Parent->AddClosure(closure);
				iterator = Closures.erase(iterator);
			} else iterator++;
		}
	}

	Environment::~Environment() {
		for (int i = 0; i < Children.size(); i++) delete Children[i];
		for (int i = 0; i < Closures.size(); i++) delete Closures[i];
		Parent->Children.erase(std::remove(Parent->Children.begin(), Parent->Children.end(), this));
	}

	SymbolicAtom XLiSpError(const char* fmt, ...) {
		va_list variadic;
		va_start(variadic, fmt);
		fprintf(stderr, COLOUR_RED);
		fprintf(stderr, "XLiSp: ");
		vfprintf(stderr, fmt, variadic);
		fprintf(stderr, COLOUR_END);
		va_end(variadic);

		Flags.CodeError = 1;
		return SymbolicAtom();
	}

	SymbolicAtom ListOfTokens::Interpret(Environment* env) {
		std::vector<TokenOrConsequences> newList = std::vector<TokenOrConsequences>();
		for (int i = 0; i < List.size(); i++) {
			if (List[i].IsConsequences) {
				std::queue<TokenContext> tokenisedStream = List[i].Interpret(env);
				while (!tokenisedStream.empty()) {
					newList.push_back(tokenisedStream.front());
					tokenisedStream.pop();
				}
			} else newList.push_back(List[i]);
		}
		return SymbolicAtom(ListOfTokens(newList));
	}

	std::queue<TokenContext> ListOfTokens::Tokenise(Environment* env) {
		std::queue<TokenContext> result;
		for (int i = 0; i < List.size(); i++) {
			result.push(List[i].GetToken());
		}
		return result;
	}

	SymbolicAtom SymbolicAtom::CastToString() {
		switch (this->Type) {
		case XLISP_NULL: return SymbolicAtom("null", false);
		case XLISP_STRING: return *this;
		case XLISP_IDENTIFIER: return SymbolicAtom(this->String, false);
		case XLISP_INTEGER: return SymbolicAtom(std::to_string(this->Integer), false);
		case XLISP_BOOLE: return SymbolicAtom(this->Truth ? "true" : "false", false);
		case XLISP_CLOSURE: return SymbolicAtom("<#closure>", false);
		case XLISP_TOKEN_LIST: return SymbolicAtom("<#token list>", false);
		default: return XLiSpError("Cannot convert to string.\n");
		case XLISP_LIST:
			std::string total = "(";
			std::vector<Symbolic> symbols = this->List.GetSymbols();
			for (int i = 0; i < symbols.size(); i++) {
				if (symbols[i].IsAtomic()) total += symbols[i].GetAtom().CastToString().String;
				else total += SymbolicAtom(symbols[i].GetList()).CastToString().String;
				if (i != symbols.size() - 1) total += ' ';
			}
			total += ')';
			return SymbolicAtom(total, false);
		}
	}
}
