#ifndef _XLISP_XLS_H_
#define _XLISP_XLS_H_
#include "Lexer.hxx"
#include "Parser.hxx"

namespace XLiSp {

	enum XLiSpType {
		XLISP_NULL = 0,
		XLISP_INTEGER = 1,
		XLISP_STRING = 2,
		XLISP_IDENTIFIER = 3,
		XLISP_LIST = 4,
		XLISP_BOOLE = 5,
		XLISP_CLOSURE = 6,
		XLISP_TOKEN_LIST = 7
	};

	class Symbolic;
	class SymbolicList;
	class SymbolicAtom;
	class Closure;
	class Environment;

	typedef SymbolicAtom(*Fun)(SymbolicList, Environment*);

	class SymbolicList {
		std::vector<Symbolic> Symbols;
	public:
		SymbolicList() {}
		SymbolicList(std::vector<Symbolic> symbols) : Symbols(symbols) {}
		std::queue<TokenContext> Tokenise(Environment* env);
		SymbolicAtom Interpret(Environment* env);
		std::vector<Symbolic> GetSymbols() { return Symbols; }
		Symbolic Expand(Environment* env);
	};

	class TokenOrConsequences {
		SymbolicList Consequences;
		TokenContext Token;
	public:
		bool IsConsequences = false;
		TokenOrConsequences() {}
		TokenOrConsequences(TokenContext ctx) : Token(ctx) {}
		TokenOrConsequences(SymbolicList csq) : Consequences(csq), IsConsequences(true) {}
		std::queue<TokenContext> Interpret(Environment* env);
		TokenContext GetToken() { return Token; }
		// Symbolic Expand(Environment* env);
	};

	class ListOfTokens {
		std::vector<TokenOrConsequences> List;
	public:
		ListOfTokens() {}
		ListOfTokens(std::vector<TokenOrConsequences> list) : List(list) {}
		std::queue<TokenContext> Tokenise(Environment* env);
		SymbolicAtom Interpret(Environment* env);
		std::vector<TokenOrConsequences> GetList() { return List; }
	};

	class SymbolicAtom {
	public:
		XLiSpType Type;
		dword Integer;
		std::string String;
		SymbolicList List;
		bool Truth = true;
		Closure* Enclosure;
		ListOfTokens TokenList;
		SymbolicAtom() : Type(XLISP_NULL), Truth(false) {}
		SymbolicAtom(dword integer) : Integer(integer), Type(XLISP_INTEGER) {}
		SymbolicAtom(std::string string, bool identifier) : String(string), Type(identifier ? XLISP_IDENTIFIER : XLISP_STRING) {}
		SymbolicAtom(SymbolicList list) : List(list), Type(XLISP_LIST) {}
		SymbolicAtom(bool truth) : Truth(truth), Type(XLISP_BOOLE) {}
		SymbolicAtom(Closure* enclosure) : Enclosure(enclosure), Type(XLISP_CLOSURE) {}
		SymbolicAtom(ListOfTokens tokenList) : TokenList(tokenList), Type(XLISP_TOKEN_LIST) {}
		SymbolicAtom CastToString();
		std::queue<TokenContext> Tokenise(Environment* env);
	};

	class Symbolic {
		bool Atomic = false;
		SymbolicAtom Atom;
		SymbolicList List;
	public:
		Symbolic() {}
		Symbolic(SymbolicAtom atom) : Atom(atom), Atomic(true) {}
		Symbolic(SymbolicList list) : List(list) {}
		Symbolic Expand(Environment* env);
		SymbolicAtom Interpret(Environment* env);
		bool IsAtomic() { return Atomic; }
		SymbolicAtom GetAtom() { return Atom; }
		SymbolicList GetList() { return List;}
	};

	class SymbolicParser {
		std::queue<TokenContext> Stream;
	public:
		SymbolicParser(std::queue<TokenContext> stream) : Stream(stream) {}
		Symbolic ParseSymbolic();
		SymbolicList ParseList();
		SymbolicAtom ParseAtom();
		SymbolicAtom ParseTokenList();
	};

	class Environment {
		Environment* Parent = nullptr;
		std::map<std::string, Symbolic> SymbolMap;
		std::vector<Environment*> Children;
		std::vector<Closure*> Closures;
	public:
		Environment() {}
		Environment(Environment* parent) : Parent(parent) { parent->AddChild(this); }
		~Environment();
		std::map<std::string, Symbolic> GetSymbolMap() { return SymbolMap; }
		void Set(std::string key, Symbolic value);
		Environment* Find(std::string key);
		Symbolic Get(std::string key);
		void AddChild(Environment* child);
		void AddClosure(Closure* closure);
		void ExheritClosure(Closure* closure);
	};

	class Closure {
		SymbolicList Arguments;
		Symbolic Body;
	public:
		Closure() {}
		Closure(SymbolicList arguments, Symbolic body) : Arguments(arguments), Body(body) {}
		SymbolicAtom Interpret(SymbolicList symbolList, Environment* env);
	};

	UQP(Expression) Evaluate(std::queue<TokenContext> InputStream);

	SymbolicAtom XLiSpError(const char* fmt, ...);
}

#endif
