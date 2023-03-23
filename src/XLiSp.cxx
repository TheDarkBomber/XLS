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
		delete newEnv;
		return result;
	}

	static SymbolicAtom EmitFun(SymbolicList symbolList, Environment* env) {
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
		SymbolicAtom operand = symbols[1].Interpret(env);
		if (operand.Type != XLISP_STRING) return XLiSpError("Expected string as input to echo.\n");
		// TODO: Cast any atom to a string type automatically for echoing.
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

	UQP(Expression) Evaluate(std::queue<TokenContext> InputStream) {
		SymbolFunctionMap["i+"] = AddFun;
		SymbolFunctionMap["i*"] = MulFun;
		SymbolFunctionMap["set!"] = SetFun;
		SymbolFunctionMap["let*"] = LetFun;
		SymbolFunctionMap["emit"] = EmitFun;
		SymbolFunctionMap["if"] = IfFun;
		SymbolFunctionMap["lambda"] = LambdaFun;
		SymbolFunctionMap["echo"] = EchoFun;
		SymbolFunctionMap["error"] = ErrorFun;
		if (!GlobalEnvironment) GlobalEnvironment = new Environment();
		UQP(SymbolicParser) parser = MUQ(SymbolicParser, InputStream);
		Symbolic symbol = Symbolic(parser->ParseList());
		TokenContext t;
		TokenStream = std::queue<TokenContext>();
		t.Value.Type = LEXEME_CHARACTER;
		t.Value.Value = '{';
		TokenStream.push(t);
		std::queue<TokenContext> OutputStream = symbol.Interpret(GlobalEnvironment).Tokenise();
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
		if (t.Value.Type == LEXEME_CHARACTER && t.Value.Value == '(') {
			Stream.pop();
			return Symbolic(this->ParseList());
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
		case LEXEME_END_OF_FILE:
		case LEXEME_CHARACTER_LITERAL:
			return XLiSpError("Attempted to understand token %d as atom.\n", t.Value.Type);
		default:
			return SymbolicAtom(t.Identifier, true);
		}
	}

	SymbolicAtom Symbolic::Interpret(Environment* env) {
		if (Atomic) {
			if (Atom.Type == XLISP_IDENTIFIER && env->Find(Atom.String))
				return env->Get(Atom.String).Interpret(env);
			return Atom;
		}
 		return List.Interpret(env);
	}

	SymbolicAtom SymbolicList::Interpret(Environment* env) {
		if (Symbols[0].IsAtomic()) {
			SymbolicAtom atom = Symbols[0].GetAtom();
			if (atom.Type == XLISP_IDENTIFIER && SymbolFunctionMap.find(atom.String) != SymbolFunctionMap.end()) {
				return SymbolFunctionMap[atom.String](*this, env);
			} else if (atom.Type == XLISP_IDENTIFIER && env->Find(atom.String)) {
				Symbolic envSymbol = env->Get(atom.String);
				Symbols[0] = envSymbol.Interpret(env);
				return this->Interpret(env);
			}

			if (atom.Type == XLISP_CLOSURE) return atom.Enclosure->Interpret(*this, env);
			return SymbolicAtom(this);
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
		newEnv->ExheritClosures();
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

	std::queue<TokenContext> SymbolicList::Tokenise() {
		std::queue<TokenContext> outstream;
		for (Symbolic symbol : Symbols) {
			std::queue<TokenContext> Q = symbol.Interpret(GlobalEnvironment).Tokenise();
			while (!Q.empty()) {
				TokenContext t = Q.front();
				outstream.push(t);
				t.Value.Type = LEXEME_CHARACTER;
				t.Value.Value = ';';
				outstream.push(t);
				Q.pop();
			}
		}
		return outstream;
	}

	std::queue<TokenContext> SymbolicAtom::Tokenise() {
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
			return List->Tokenise();
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
		default: exit(3054);
		}
		return outstream;
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

	void Environment::ExheritClosures() {
		for (int i = 0; i < Closures.size(); i++) Parent->AddClosure(Closures[i]);
		Closures.clear();
	}

	Environment::~Environment() {
		for (int i = 0; i < Children.size(); i++) delete Children[i];
		for (int i = 0; i < Closures.size(); i++) delete Closures[i];
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
}
