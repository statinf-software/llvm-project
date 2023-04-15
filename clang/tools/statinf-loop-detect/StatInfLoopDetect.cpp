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

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <algorithm>

using namespace clang::driver;
using namespace clang::tooling;
using namespace llvm;
using namespace std;
namespace am = clang::ast_matchers;
using MatchResult = am::MatchFinder::MatchResult;
using MatchCallback = am::MatchFinder::MatchCallback;

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp(
    "To use this tool ...:\n"
    "Options:\n"
);

static cl::OptionCategory OptCat("statinf-instrumentation options");
static const opt::OptTable &Options = getDriverOptTable();
static cl::list<std::string>
    IncludePath("I", cl::desc(Options.getOptionHelpText(options::OPT_include)),
                  cl::cat(OptCat));
static cl::list<std::string>
    Definitions("D", cl::desc(Options.getOptionHelpText(options::OPT_defsym)),
                  cl::cat(OptCat));
static cl::opt<std::string>
    Out(
      "o",
      cl::desc("File in which storing the output"),
      cl::cat(OptCat)
    );
static cl::list<std::string>
    InputDir("input-dir", 
      cl::desc("Recursively scans this directory to find all .c files, also add all found directories in the include path"),
      cl::cat(OptCat)
    );

                
namespace {

class LoopDetect : public MatchCallback {
  std::map<std::string, SmallVector<clang::DoStmt*>> doloop;
  std::map<std::string, SmallVector<clang::ForStmt*>> forloop;
  std::map<std::string, SmallVector<clang::WhileStmt*>> whileloop;

  std::string current_func;

  clang::SourceManager *sm = nullptr;
public:
  explicit LoopDetect() {}

  virtual void run(const MatchResult &Result) {
    if(!sm)
      sm = Result.SourceManager;
    if (const clang::FunctionDecl *F = Result.Nodes.getNodeAs<clang::FunctionDecl>("func")) {
      current_func = F->getName().str();
    }
    else if (const clang::DoStmt *S = Result.Nodes.getNodeAs<clang::DoStmt>("dostmt")) {
      doloop[current_func].push_back(const_cast<clang::DoStmt*>(S));
    }
    else if (const clang::ForStmt *S = Result.Nodes.getNodeAs<clang::ForStmt>("forstmt")) {
      forloop[current_func].push_back(const_cast<clang::ForStmt*>(S));
    }
    else if (const clang::WhileStmt *S = Result.Nodes.getNodeAs<clang::WhileStmt>("whilestmt")) {
      whileloop[current_func].push_back(const_cast<clang::WhileStmt*>(S));
    }
  }

  template<class Ty>
  std::string getMapToJson(std::map<std::string, SmallVector<Ty>> loops) {
    std::string loop_str = "";
    for(auto fn : loops) {
      loop_str += "\t\t\""+fn.first+"\": [\n";
      for(auto loop : fn.second) {
        std::string loc = loop->getBeginLoc().printToString(*sm);

        size_t pos = loc.find_first_of(":");
        std::string file = loc.substr(0, pos);
        
        size_t pos2 = loc.find_first_of(":", pos+1);
        std::string line = loc.substr(pos+1, pos2-pos-1);
        
        std::string maxcount = "-1";
        
        loop_str += "\t\t\t{\n";
        loop_str += "\t\t\t\t\"file\": "+file+"\",\n";
        loop_str += "\t\t\t\t\"line\": "+line+",\n";
        loop_str += "\t\t\t\t\"maxcount\": "+maxcount+"\n";
        loop_str += "\t\t\t},\n";
      }
      loop_str = loop_str.substr(0, loop_str.size()-2);
      loop_str += "\n\t\t],\n";
    }
    return loop_str.substr(0, loop_str.size()-2) + "\n";
  }

  void print(llvm::raw_ostream &OS) {
    OS << "{\n";
    OS << "\t\"do loop\": [\n";
    OS << getMapToJson(doloop);
    OS << "\t],\n";
    OS << "\t\"for loop\": [\n";
    OS << getMapToJson(forloop);
    OS << "\t],\n";
    OS << "\t\"while loop\": [\n";
    OS << getMapToJson(whileloop);
    OS << "\t]\n";
    OS << "}";
  }
};

static void scandir(llvm::vfs::FileSystem &fs, StringRef dirname, cl::list<string> &dirs, vector<string> &C_files) {
  dirs.push_back(*(getAbsolutePath(fs, dirname)));
  std::error_code EC;
  for(llvm::vfs::directory_iterator elt = fs.dir_begin(dirname, EC), dirend ; elt != dirend && !EC; elt.increment(EC)) {
    if (elt->path().endswith(".c"))
      C_files.push_back(*(getAbsolutePath(fs, elt->path())));
    else if(elt->type() == llvm::sys::fs::file_type::directory_file)
      scandir(fs, elt->path(), dirs, C_files);
  }
}

} // namespace

int main(int argc, const char **argv) {
    llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

    auto ExpectedParser =
        CommonOptionsParser::create(argc, argv, OptCat, llvm::cl::NumOccurrencesFlag::ZeroOrMore);
    if (!ExpectedParser) {
        llvm::errs() << ExpectedParser.takeError();
        return 1;
    }
    CommonOptionsParser &OptionsParser = ExpectedParser.get();

    vector<string> args{"-Wno-int-conversion", 
    "-Wno-unused-value", 
    "-Wno-implicit-function-declaration", 
    "-Wno-shift-count-overflow",
    "-Wno-parentheses-equality",
    "-Wno-main-return-type",
    "-Wno-missing-declarations"};

    shared_ptr<clang::PCHContainerOperations> PCHContainerOps = make_shared<clang::PCHContainerOperations>();
    std::vector<std::string> AbsolutePaths;
    llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem> OverlayFileSystem(
      new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem())
    );

    // Compute all absolute paths before we run any actions, as those will change
    // the working directory.
    AbsolutePaths.reserve(OptionsParser.getSourcePathList().size());
    for (const auto &SourcePath : OptionsParser.getSourcePathList()) {
      auto AbsPath = getAbsolutePath(*OverlayFileSystem, SourcePath);
      if (!AbsPath) {
        llvm::errs() << "Skipping " << SourcePath
                    << ". Error while getting an absolute path: "
                    << llvm::toString(AbsPath.takeError()) << "\n";
        continue;
      }
      AbsolutePaths.push_back(std::move(*AbsPath));
    }

    // Scan for additional C files and directories to put in the include paths from a given root project
    for(string edir : InputDir) {
      scandir(*OverlayFileSystem, edir, IncludePath, AbsolutePaths);
    }

    // Add include paths
    for(auto I : IncludePath) {
      auto AbsPath = getAbsolutePath(*OverlayFileSystem, I);
      if (!AbsPath) {
        llvm::errs() << "Skipping " << I
                    << ". Error while getting an absolute path: "
                    << llvm::toString(AbsPath.takeError()) << "\n";
        continue;
      }
      args.push_back("-I"+string(AbsPath->c_str()));
    }

    // Add def symbols
    for(auto D : Definitions) {
      args.push_back("-D"+D);
    }

      //Build an empty AST
    std::unique_ptr<clang::ASTUnit> ast = buildASTFromCode("", "empty.c");

    //Build all other ASTs and merge them into the empty one
    for (llvm::StringRef File : AbsolutePaths) {
      //get file content
      std::ifstream ifs(File.str());
      std::string content( (std::istreambuf_iterator<char>(ifs) ),
                        (std::istreambuf_iterator<char>()    ) );
      clang::tooling::FileContentMappings files;
      files.push_back(make_pair(File.str(), content));

      //build ast
      std::unique_ptr<clang::ASTUnit> tmp_ast = buildASTFromCodeWithArgs(
          content, args, File.str(), "statinf-CFG-with-trace", PCHContainerOps,
          clang::tooling::getClangStripDependencyFileAdjuster(), files
      );
      if(tmp_ast == nullptr) {
        llvm::errs() << "No AST have been built for " << File.str() << "\n";
        return -1;
      }

      //import each toplevel declaration one by one
      clang::ASTImporter Importer(ast->getASTContext(), ast->getFileManager(),
                      tmp_ast->getASTContext(), tmp_ast->getFileManager(),
                      /*MinimalImport=*/false);
      for(auto decl : tmp_ast->getASTContext().getTranslationUnitDecl()->decls()) {
        auto ImportedOrErr = Importer.Import(decl);
        if (!ImportedOrErr) {
          llvm::Error Err = ImportedOrErr.takeError();
          llvm::errs() << "ERROR: " << Err << "\n";
          consumeError(std::move(Err));
          return 1;
        }
      }
    }

    // Extract the call graph from the given entrypoint
    am::DeclarationMatcher func = am::functionDecl(am::anything()).bind("func");
    am::StatementMatcher m_dostmt = am::doStmt(am::anything()).bind("dostmt");
    am::StatementMatcher m_forstmt = am::forStmt(am::anything()).bind("forstmt");
    am::StatementMatcher m_whilestmt = am::whileStmt(am::anything()).bind("whilestmt");
    LoopDetect loop_detect;
    am::MatchFinder Finder;
    Finder.addMatcher(func, &loop_detect);
    Finder.addMatcher(m_dostmt, &loop_detect);
    Finder.addMatcher(m_forstmt, &loop_detect);
    Finder.addMatcher(m_whilestmt, &loop_detect);
    Finder.matchAST(ast->getASTContext());

    std::unique_ptr<llvm::raw_ostream> OutFile = nullptr;
    if(!Out.empty()) {
      std::error_code EC;
      OutFile = std::make_unique<llvm::raw_fd_ostream>(Out, EC);
      if (EC) {
        llvm::errs() << EC.message() << "\n";
        return -1;
      }
    }

    loop_detect.print(OutFile ? *(OutFile.get()) : llvm::outs());

    return 0;
}