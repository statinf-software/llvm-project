
#include <vector>
#include <string>
#include <fstream>

#include "clang/Tooling/Tooling.h"
#include "clang/Driver/Options.h"
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
    std::vector<std::string> &C_files, std::vector<std::string> &other_files, 
    std::string Filter);