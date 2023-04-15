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
  SmallVector<std::string> doloop;
  SmallVector<std::string> forloop;
  SmallVector<std::string> whileloop;

public:
  explicit LoopDetect() {}

  virtual void run(const MatchResult &Result) {
    if (const clang::DoStmt *S = Result.Nodes.getNodeAs<clang::DoStmt>("dostmt")) {
      doloop.push_back(S->getBeginLoc().printToString(*(Result.SourceManager)));
    }
    else if (const clang::ForStmt *S = Result.Nodes.getNodeAs<clang::ForStmt>("forstmt")) {
      forloop.push_back(S->getBeginLoc().printToString(*(Result.SourceManager)));
    }
    else if (const clang::WhileStmt *S = Result.Nodes.getNodeAs<clang::WhileStmt>("whilestmt")) {
      whileloop.push_back(S->getBeginLoc().printToString(*(Result.SourceManager)));
    }
  }

  void print(llvm::raw_ostream &OS) {
    std::string doloop_str = "";
    for(auto s : doloop)
      doloop_str += "\t\t\""+s+"\",\n";
    std::string forloop_str = "";
    for(auto s : forloop)
      forloop_str += "\t\t\""+s+"\",\n";
    std::string whileloop_str = "";
    for(auto s : whileloop)
      whileloop_str += "\t\t\""+s+"\",\n";

    OS << "{\n";
    OS << "\t\"do loop\": [\n";
    OS << doloop_str.substr(0, doloop_str.size()-2) << "\n";
    OS << "\t],\n";
    OS << "\t\"for loop\": [\n";
    OS << forloop_str.substr(0, forloop_str.size()-2) << "\n";
    OS << "\t],\n";
    OS << "\t\"while loop\": [\n";
    OS << whileloop_str.substr(0, whileloop_str.size()-2) << "\n";
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
    am::StatementMatcher m_dostmt = am::doStmt(am::anything()).bind("dostmt");
    am::StatementMatcher m_forstmt = am::forStmt(am::anything()).bind("forstmt");
    am::StatementMatcher m_whilestmt = am::whileStmt(am::anything()).bind("whilestmt");
    LoopDetect loop_detect;
    am::MatchFinder Finder;
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