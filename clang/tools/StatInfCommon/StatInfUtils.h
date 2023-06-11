
#include <vector>
#include <string>
#include <fstream>

#include "clang/Tooling/Tooling.h"
#include "clang/Driver/Options.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Analysis/CallGraph.h"
#include "clang/AST/ASTImporter.h"
#include "StatInfDiagConsumer.h"

extern std::vector<std::string> args_for_clangtool;

class CallGraphExtract : public clang::ast_matchers::MatchFinder::MatchCallback {
  clang::CallGraph *callgraph;
public:
  explicit CallGraphExtract(clang::CallGraph *cg) : callgraph(cg) {}

  virtual void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override {
    if (const clang::FunctionDecl *FS = Result.Nodes.getNodeAs<clang::FunctionDecl>("entrypoint")) {
      callgraph->VisitFunctionDecl(const_cast<clang::FunctionDecl*>(FS));
    }
  }
};
void extractCallGraph(clang::ASTUnit *ast, clang::CallGraph *cg, std::string EntryPoint);

void add_include_paths_in_clangtoolarg(llvm::cl::list<std::string> &paths, llvm::vfs::FileSystem &FS);
void add_defs_in_clangtoolarg(llvm::cl::list<std::string> &defs);

int build_ast_book(std::map<llvm::StringRef, std::unique_ptr<clang::ASTUnit>> &ast_book, 
  clang::ASTUnit *app_ast, 
  const std::vector<std::string> &files, const std::vector<std::string> &args,
  clang::DiagnosticConsumer *diagcons);

std::string get_full_path(std::string dir);
std::string get_create_full_path(std::string dir, llvm::vfs::FileSystem &FS);
std::error_code create_directory_recursive(llvm::StringRef dir);
void scandir(llvm::vfs::FileSystem &fs, llvm::StringRef dirname, llvm::cl::list<std::string> &dirs, 
    std::vector<std::string> &C_files, std::vector<std::string> &other_files, std::map<std::string,std::string> &pp_c_match, 
    std::string Filter);

void get_c_files_from_cmdline(std::vector<std::string> &C_files, llvm::vfs::FileSystem &FS, clang::tooling::CommonOptionsParser &OptionsParser);