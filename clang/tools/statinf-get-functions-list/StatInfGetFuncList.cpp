#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Signals.h"
#include "clang/AST/StatInfInstrDeclPrinter.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Tooling/ArgumentsAdjusters.h"
#include "clang/Analysis/CallGraph.h"
#include "clang/AST/ASTImporter.h"

#include "../StatInfCommon/StatInfUtils.h"

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <algorithm>


using namespace llvm;
using namespace clang;
using namespace std;
namespace cd = clang::driver;
namespace ct = clang::tooling;
namespace am = clang::ast_matchers;
using MatchResult = am::MatchFinder::MatchResult;
using MatchCallback = am::MatchFinder::MatchCallback;

static cl::extrahelp CommonHelp(ct::CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp(
    "To use this tool ...:\n"
    "Options:\n"
);

static cl::OptionCategory OptCat("statinf-instrumentation options");
static const opt::OptTable &Options = cd::getDriverOptTable();
static cl::list<string>
    IncludePath("I", cl::desc(Options.getOptionHelpText(cd::options::OPT_include)),
                  cl::cat(OptCat));
static cl::list<string>
    Definitions("D", cl::desc(Options.getOptionHelpText(cd::options::OPT_defsym)),
                  cl::cat(OptCat));
static cl::opt<string>
    OutFile(
      "o",
      cl::desc("File in which storing the output"),
      cl::cat(OptCat)
    );
static cl::opt<string>
    InputDir("input-dir", 
      cl::desc("Recursively scans this directory to find all .c files, also add all found directories in the include path"),
      cl::cat(OptCat)
    );
static cl::opt<string>
    Filter("filter",
      cl::desc("Filter out files that matches the given pattern, the full path is checked so a full directory can been filter out."),
      cl::cat(OptCat)
    );
                
namespace {

class GetFunctionsList : public MatchCallback {
public:
  vector<FunctionDecl*> func_list;
  explicit GetFunctionsList() {}

  virtual void run(const MatchResult &Result) {
    if (const FunctionDecl *F = Result.Nodes.getNodeAs<FunctionDecl>("func"))
      func_list.push_back(const_cast<FunctionDecl*>(F));
  }
};

static void scandir(vfs::FileSystem &fs, StringRef dirname, cl::list<string> &dirs, vector<string> &C_files) {
  dirs.push_back(*(ct::getAbsolutePath(fs, dirname)));
  error_code EC;
  for(vfs::directory_iterator elt = fs.dir_begin(dirname, EC), dirend ; elt != dirend && !EC; elt.increment(EC)) {
    if (elt->path().endswith(".c"))
      C_files.push_back(*(ct::getAbsolutePath(fs, elt->path())));
    else if(elt->type() == sys::fs::file_type::directory_file)
      scandir(fs, elt->path(), dirs, C_files);
  }
}

static void printListToJson(raw_ostream &Out, const ASTContext &ctx, vector<FunctionDecl*> flist, const string &inputdir) {
  string sout;
  raw_string_ostream sstream(sout);
  
  for(FunctionDecl* func : flist) {
    string loc = func->getBeginLoc().printToString(ctx.getSourceManager());

    size_t pos = loc.find_first_of(":");
    string file = loc.substr(0, pos);
    file = replaceAll(file, inputdir, "");
    
    size_t pos2 = loc.find_first_of(":", pos+1);
    string line = loc.substr(pos+1, pos2-pos-1);

    sstream << "\t\"" << func->getName() << "\": {\n";
    sstream << "\t\t\"file\": \""<<file<<"\",\n";
    sstream << "\t\t\"line\": \""<<line<<"\"\n";
    sstream << "\t},\n";
  }

  Out << "{\n";
  Out << sout.substr(0, sout.size()-2) << "\n";
  Out << "}";
}

} // namespace

int main(int argc, const char **argv) {
    sys::PrintStackTraceOnErrorSignal(argv[0]);

    auto ExpectedParser =
        ct::CommonOptionsParser::create(argc, argv, OptCat, cl::NumOccurrencesFlag::ZeroOrMore);
    if (!ExpectedParser) {
        errs() << ExpectedParser.takeError();
        return 1;
    }
    ct::CommonOptionsParser &OptionsParser = ExpectedParser.get();

    shared_ptr<PCHContainerOperations> PCHContainerOps = make_shared<PCHContainerOperations>();
    vector<string> C_files, other_files;
    map<string,string> pp_c_match;
    IntrusiveRefCntPtr<vfs::OverlayFileSystem> OverlayFileSystem(
      new vfs::OverlayFileSystem(vfs::getRealFileSystem())
    );

    get_c_files_from_cmdline(C_files, *OverlayFileSystem, OptionsParser);

    if(!InputDir.empty() && !C_files.empty()) {
      errs() << "Can't provide a list of files and an input-dir to scan for source files.\n";
      return 1;
    }

    // Scan for additional C files and directories to put in the include paths from a given root project
    scandir(*OverlayFileSystem, InputDir, IncludePath, C_files, other_files, pp_c_match, Filter);

    add_include_paths_in_clangtoolarg(IncludePath, *OverlayFileSystem);
    add_defs_in_clangtoolarg(Definitions);

      //Build an empty AST
    unique_ptr<ASTUnit> ast = ct::buildASTFromCode("", "empty.c");
    map<StringRef, unique_ptr<ASTUnit>> ast_books;

    IntrusiveRefCntPtr<DiagnosticOptions> diagopts = new DiagnosticOptions;
    diagopts->ShowColors = true;
    StatInfDiagnosticPrinter diagprinter(llvm::errs(), diagopts);
    //Build all other ASTs and merge them into the empty one
    build_ast_book(ast_books, ast.get(), C_files, args_for_clangtool, &diagprinter);

    string full_input_dir_path = "";
    if(!InputDir.empty()) {
      SmallVector<char> tmp_path;
      sys::fs::real_path(InputDir, tmp_path, true);
      full_input_dir_path = string(tmp_path.data(), tmp_path.size());
      if(full_input_dir_path.at(full_input_dir_path.size()-1) != '/')
        full_input_dir_path += "/";
    }

    // Extract function list
    am::DeclarationMatcher func = am::functionDecl(am::anything()).bind("func");
    GetFunctionsList get_func;
    am::MatchFinder Finder;
    Finder.addMatcher(func, &get_func);
    Finder.matchAST(ast->getASTContext());

    if(!OutFile.empty()) {
      error_code EC;
      raw_fd_ostream Out(OutFile, EC);
      if (EC) {
        errs() << OutFile << ": " << EC.message() << "\n";
        return -1;
      }
      printListToJson(Out, ast->getASTContext(), get_func.func_list, full_input_dir_path);
    }
    else
      printListToJson(llvm::outs(), ast->getASTContext(), get_func.func_list, full_input_dir_path);

    return 0;
}