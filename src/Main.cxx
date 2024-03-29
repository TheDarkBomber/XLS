#include "Lexer.hxx"
#include "Parser.hxx"
#include "Debug.hxx"
#include "colours.def.h"
#include <llvm/ADT/Optional.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/Host.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <map>
#include <stdio.h>
#include <system_error>

extern dword SpacesDetected;

extern llvm::DataLayout* GlobalLayout;
extern llvm::Triple* GlobalTriple;
extern ParserFlags Flags;

extern std::map<std::string, Precedence> BinaryPrecedence;
extern Token CurrentToken;
extern Token GetNextToken();

extern UQP(llvm::Module) GlobalModule;

static void Handle() {
	for(;;) {
		switch (CurrentToken.Type) {
		case LEXEME_END_OF_FILE:
			return;
		case LEXEME_IMPLEMENT:
			HandleImplementation();
			break;
		case LEXEME_EXTERN:
			HandleExtern();
			break;
		case LEXEME_OPERATOR:
			HandleOperatorDefinition();
			break;
		case LEXEME_IDENTIFIER:
			HandleGlobal();
			break;
		case LEXEME_STRUCT:
			HandleStruct();
			break;
		case LEXEME_TYPEDEF:
			HandleTypedef();
			break;
		case LEXEME_FUNCDEF:
			HandleFuncdef();
			break;
		default:
			switch (CurrentToken.Value) {
			case ';':
				GetNextToken();
				break;
			default:
				HandleUnboundedExpression();
				break;
			}
		}
	}
}

int main(int argc, char* argv[]) {
	BinaryPrecedence["="] = PRECEDENCE_ASSIGN;
	BinaryPrecedence["<"] = PRECEDENCE_COMPARE;
	BinaryPrecedence[">"] = PRECEDENCE_COMPARE;
	BinaryPrecedence["+"] = PRECEDENCE_ADD;
	BinaryPrecedence["-"] = PRECEDENCE_ADD;
	BinaryPrecedence["+/"] = PRECEDENCE_ADD;
	BinaryPrecedence["*"] = PRECEDENCE_MULTIPLY;
	BinaryPrecedence["%"] = PRECEDENCE_MULTIPLY;
	BinaryPrecedence["/"] = PRECEDENCE_MULTIPLY;
	BinaryPrecedence["*/"] = PRECEDENCE_MULTIPLY;
	BinaryPrecedence["=="] = PRECEDENCE_COMPARE;
	BinaryPrecedence["!="] = PRECEDENCE_COMPARE;
	BinaryPrecedence["&&"] = PRECEDENCE_LOGICAL;
	BinaryPrecedence["||"] = PRECEDENCE_LOGICAL;
	BinaryPrecedence["^^"] = PRECEDENCE_LOGICAL;
	BinaryPrecedence["&"] = PRECEDENCE_BITWISE;
	BinaryPrecedence["|"] = PRECEDENCE_BITWISE;
	BinaryPrecedence["^"] = PRECEDENCE_BITWISE;
	BinaryPrecedence["~"] = PRECEDENCE_BITWISE;
	BinaryPrecedence["&/"] = PRECEDENCE_BITWISE;
	BinaryPrecedence["|/"] = PRECEDENCE_BITWISE;
	BinaryPrecedence["^/"] = PRECEDENCE_BITWISE;
	BinaryPrecedence["!"] = PRECEDENCE_LOGICAL;
	BinaryPrecedence["<="] = PRECEDENCE_COMPARE;
	BinaryPrecedence[">="] = PRECEDENCE_COMPARE;
	BinaryPrecedence["|>"] = PRECEDENCE_PIPE;

	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

	bool outputASM = false;
	bool ignoreHandleErrors = false;
	bool emitIR = false;
	std::string targetTriple = llvm::sys::getDefaultTargetTriple();
	std::string MTune = "generic";
	std::string filename = "output.o";
	std::string moduleName = filename;

	unsigned currentargc = 1;
#define PARSE_ARGV(comparison) if (!std::string(comparison).compare(argv[currentargc]))
#define SET_ARGV(value) if (argc <= currentargc + 1) return 1; currentargc++; value = argv[currentargc];
	#define END_ARGV goto continue_argv_parse
	while (argc > currentargc) {
		PARSE_ARGV("march") {
			SET_ARGV(targetTriple);
			END_ARGV;
		}

		PARSE_ARGV("mtune") {
			SET_ARGV(MTune);
			END_ARGV;
		}

		PARSE_ARGV("asm") {
			outputASM = true;
			END_ARGV;
		}

		PARSE_ARGV("in") {
			SET_ARGV(Dbg.Filename);
			END_ARGV;
		}

		PARSE_ARGV("out") {
			SET_ARGV(filename);
			END_ARGV;
		}

		PARSE_ARGV("dir") {
			SET_ARGV(Dbg.Directory);
			END_ARGV;
		}

		PARSE_ARGV("ir") {
			emitIR = true;
			END_ARGV;
		}

		PARSE_ARGV("ignorehandleerrors") {
			ignoreHandleErrors = true;
			END_ARGV;
		}

		PARSE_ARGV("module") {
			SET_ARGV(moduleName);
			END_ARGV;
		}

		PARSE_ARGV("pessimise") {
			Flags.NoOptimise = 1;
			END_ARGV;
		}

		PARSE_ARGV("debug") {
			Flags.Debug = 1;
			END_ARGV;
		}

	continue_argv_parse:
		currentargc++;
	}
	#undef PARSE_ARGV
	#undef SET_ARGV
	#undef END_ARGV

	std::string error;
	const llvm::Target *target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

	if (!target) {
		llvm::errs() << COLOUR_RED_BOLD << error << COLOUR_END;
		return 1;
	}
	std::string features = "";

	llvm::TargetOptions options;
	llvm::Optional<llvm::Reloc::Model> RM = llvm::Reloc::DynamicNoPIC;
	llvm::TargetMachine *targetMachine = target->createTargetMachine(targetTriple, MTune, features, options, RM);

	llvm::DataLayout layout = targetMachine->createDataLayout();
	GlobalLayout = &layout;
	GlobalTriple = (llvm::Triple*)&targetMachine->getTargetTriple();

	InitialiseModule(moduleName);
	GetNextToken();
	Handle();

	if (Flags.ParseError == 1) {
		llvm::errs() << COLOUR_RED_BOLD << "Parse errors encountered during compilation." << (!ignoreHandleErrors ? " Stop." : "") << COLOUR_END << "\n";
		if (!ignoreHandleErrors) return 1;
	}

	if (Flags.CodeError == 1) {
		llvm::errs() << COLOUR_RED_BOLD "Code render errors encountered during compilation." << (!ignoreHandleErrors ? " Stop." : "") << COLOUR_END << "\n";
		if (!ignoreHandleErrors) return 1;
	}

	llvm::errs() << COLOUR_BLUE << "Building for " << COLOUR_BLUE_BOLD << targetTriple << COLOUR_END << "\n";

	if (Flags.Debug) Dbg.Builder->finalize();

	GlobalModule->setTargetTriple(targetTriple);
	GlobalModule->setDataLayout(layout);

	std::error_code errorCode;
	llvm::raw_fd_ostream destination(filename, errorCode, llvm::sys::fs::OF_None);

	if (errorCode) {
		llvm::errs() << COLOUR_RED_BOLD << "Failed to open file: " << errorCode.message() << COLOUR_END;
		return 1;
	}

	llvm::legacy::PassManager pass;
	llvm::CodeGenFileType fileType = outputASM ? llvm::CGFT_AssemblyFile : llvm::CGFT_ObjectFile;

	if (targetMachine->addPassesToEmitFile(pass, destination, nullptr, fileType)) {
		llvm::errs() << COLOUR_RED_BOLD << "Cannot emit file type for " << MTune << COLOUR_END;
		return 1;
	}

	if (emitIR) GlobalModule->print(llvm::outs(), nullptr);
	pass.run(*GlobalModule);
	destination.flush();

	if (SpacesDetected > 0) {
		fprintf(stderr, COLOUR_PURPLE_BOLD);
		fprintf(stderr, "Accessibility warning\n");
		fprintf(stderr, COLOUR_RED);
		fprintf(stderr, "%u line%s detected containing spaces for indentation.\n", SpacesDetected, SpacesDetected == 1 ? " was" : "s were");
		fprintf(stderr, "Please consider using tabs to enable accessibility within your codebase.\n");
		fprintf(stderr, COLOUR_END);
	}

	llvm::errs() << COLOUR_GREEN << "Wrote " << filename << " for " << MTune << "\n" << COLOUR_END;

	return 0;
}
