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
		XLISP_CLOSURE = 6
	};

	class Symbolic;
	class SymbolicList;
	class SymbolicAtom;
	class Closure;
	class Environment;

	typedef SymbolicAtom(*Fun)(SymbolicList, Environment*);

	class SymbolicAtom {
	public:
		XLiSpType Type;
		dword Integer;
		std::string String;
		SymbolicList* List;
		bool Truth = true;
		Closure* Enclosure;
		SymbolicAtom() : Type(XLISP_NULL), Truth(false) {}
		SymbolicAtom(dword integer) : Integer(integer), Type(XLISP_INTEGER) {}
		SymbolicAtom(std::string string, bool identifier) : String(string), Type(identifier ? XLISP_IDENTIFIER : XLISP_STRING) {}
		SymbolicAtom(SymbolicList* list) : List(list), Type(XLISP_LIST) {}
		SymbolicAtom(bool truth) : Truth(truth), Type(XLISP_BOOLE) {}
		SymbolicAtom(Closure* enclosure) : Enclosure(enclosure), Type(XLISP_CLOSURE) {}
		std::queue<TokenContext> Tokenise();
	};

	class SymbolicList {
		std::vector<Symbolic> Symbols;
	public:
		SymbolicList() {}
		SymbolicList(std::vector<Symbolic> symbols) : Symbols(symbols) {}
		std::queue<TokenContext> Tokenise();
		SymbolicAtom Interpret(Environment* env);
		std::vector<Symbolic> GetSymbols() { return Symbols; }
		Symbolic Expand(Environment* env);
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
		void ExheritClosures();
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
