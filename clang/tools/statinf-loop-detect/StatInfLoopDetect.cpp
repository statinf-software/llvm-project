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
    Out(
      "o",
      cl::desc("File in which storing the output"),
      cl::cat(OptCat)
    );
static cl::list<string>
    InputDir("input-dir", 
      cl::desc("Recursively scans this directory to find all .c files, also add all found directories in the include path"),
      cl::cat(OptCat)
    );

                
namespace {

class LoopDetect : public MatchCallback {
  map<string, SmallVector<pair<DoStmt*, int64_t>>> doloop;
  map<string, SmallVector<pair<ForStmt*, int64_t>>> forloop;
  map<string, SmallVector<pair<WhileStmt*, int64_t>>> whileloop;

  string current_func;

  SourceManager *sm = nullptr;
public:
  explicit LoopDetect() {}

  virtual void run(const MatchResult &Result) {
    if(!sm)
      sm = Result.SourceManager;
    if (const FunctionDecl *F = Result.Nodes.getNodeAs<FunctionDecl>("func")) {
      current_func = F->getName().str();
    }
    else if (const DoStmt *S = Result.Nodes.getNodeAs<DoStmt>("dostmt")) {
      doloop[current_func].push_back({const_cast<DoStmt*>(S), -1});
    }
    else if (const ForStmt *S = Result.Nodes.getNodeAs<ForStmt>("forstmt")) {
      ForStmt *loop = const_cast<ForStmt*>(S);
      string loc = loop->getBeginLoc().printToString(*sm);
      auto initval = getForInitValue(loop, *Result.Context);
      auto condval = getForCondValue(loop, *Result.Context);
      auto incval = getForIncValue(loop, *Result.Context);
      uint64_t bound = -1;
      if(initval && condval && incval) {
        bound = (initval < condval)
                            ? floor((*condval - *initval) / (double)*incval)
                            : floor((*initval - *condval) / (double)*incval);
      }
      forloop[current_func].push_back({loop, bound});
    }
    else if (const WhileStmt *S = Result.Nodes.getNodeAs<WhileStmt>("whilestmt")) {
      whileloop[current_func].push_back({const_cast<WhileStmt*>(S), -1});
    }
  }

  template<class Ty>
  string getMapToJson(map<string, SmallVector<pair<Ty, int64_t>>> loops) {
    string loop_str = "";
    for(auto fn : loops) {
      loop_str += "\t\t\""+fn.first+"\": [\n";
      for(auto loop : fn.second) {
        string loc = loop.first->getBeginLoc().printToString(*sm);

        size_t pos = loc.find_first_of(":");
        string file = loc.substr(0, pos);
        
        size_t pos2 = loc.find_first_of(":", pos+1);
        string line = loc.substr(pos+1, pos2-pos-1);
        
        string maxcount = to_string(loop.second);
        
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

  void print(raw_ostream &OS) {
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

  Optional<uint64_t> getForInitValue(ForStmt *loop, const ASTContext &Ctx) {
    string loc = loop->getBeginLoc().printToString(*sm);
    Stmt *init = loop->getInit();
    if(DeclStmt *dsinit = dyn_cast<DeclStmt>(init)) {
      if(VarDecl *vdinit = dyn_cast<VarDecl>(dsinit->getSingleDecl())) {
        return getLiteralValue(vdinit->getInit(), Ctx, loc, "For (VarDecl) Init");
      }
      errs() << "line " << loc << ": For Init is a DeclStmt but not a VarDecl ("<<dsinit->getSingleDecl()->getDeclKindName()<<")\n";
      return None;
    }
    
    if (BinaryOperator *binit = dyn_cast<BinaryOperator>(init)) {
      return getBinaryOpConstantVal(binit, Ctx, loc, "For Init");
    }

    errs() << "line " << loc << ": For Init Value is not a DeclStmt/BinaryOperator ("<<init->getStmtClassName()<<")\n";
    return None;
  }

  Optional<uint64_t> getForCondValue(ForStmt *loop, const ASTContext &Ctx) {
    string loc = loop->getBeginLoc().printToString(*sm);
    Stmt *cond = loop->getCond();
    if (BinaryOperator *bcond = dyn_cast<BinaryOperator>(cond)) {
      return getBinaryOpConstantVal(bcond, Ctx, loc, "For Cond");
    }

    errs() << "line " << loc << ": For Cond Value is not a BinaryOperator ("<<cond->getStmtClassName()<<")\n";
    return None;
  }

  Optional<uint64_t> getForIncValue(ForStmt *loop, const ASTContext &Ctx) {
    string loc = loop->getBeginLoc().printToString(*sm);
    Stmt *inc = loop->getInc();
    if (BinaryOperator *binit = dyn_cast<BinaryOperator>(inc)) {
      if(auto val = getBinaryOpConstantVal(binit, Ctx, loc, ""))
        return val;
      errs() << "line " << loc << ": For Inc is a expr too complicated to be easily parsed\n";
      return None;
    }

    if(UnaryOperator *uinit = dyn_cast<UnaryOperator>(inc)) {
      switch (uinit->getOpcode()) {
        case UO_PostInc:
        case UO_PostDec:
        case UO_PreInc:
        case UO_PreDec:
          return 1;
        default:
          errs() << "line " << loc << ": For Inc contains an unknown UnaryOperator ("<<UnaryOperator::getOpcodeStr(uinit->getOpcode())<<")\n";
          return None;
      }
    }

    errs() << "line " << loc << ": For Cond Value is not a BinaryOperator/UnaryOperator ("<<inc->getStmtClassName()<<")\n";
    return None;
  }

  Optional<uint64_t> getLiteralValue(Expr *expr, const ASTContext &Ctx, const string &loc, const string &errormsg) {
    if(IntegerLiteral *iinit = dyn_cast<IntegerLiteral>(expr)) {
      if(auto oinit = iinit->getIntegerConstantExpr(Ctx)) {
        return oinit->getLimitedValue();
      }
      if(!errormsg.empty()) errs() << "line " << loc << ": "<<errormsg<<" is an integer literal but doesn't have a value\n";
      return None;
    }
    if(!errormsg.empty()) errs() << "line " << loc << ": "<<errormsg<<" expr is not an integer literal ("<<expr->getStmtClassName()<<")\n";
    return None;
  }

  Optional<uint64_t> getBinaryOpConstantVal(BinaryOperator *binit, const ASTContext &Ctx, const string &loc, const string &errormsg) {
    if(auto val = getLiteralValue(binit->getLHS(), Ctx, loc, ""))
      return val;
    if(auto val = getLiteralValue(binit->getRHS(), Ctx, loc, ""))
      return val;
    if(!errormsg.empty()) errs() << "line " << loc << ": "<<errormsg<<" with binary operator doesn't include an integer literal\n"; 
    return None;
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

    vector<string> args{"-Wno-int-conversion", 
    "-Wno-unused-value", 
    "-Wno-implicit-function-declaration", 
    "-Wno-shift-count-overflow",
    "-Wno-parentheses-equality",
    "-Wno-main-return-type",
    "-Wno-missing-declarations"};

    shared_ptr<PCHContainerOperations> PCHContainerOps = make_shared<PCHContainerOperations>();
    vector<string> AbsolutePaths;
    IntrusiveRefCntPtr<vfs::OverlayFileSystem> OverlayFileSystem(
      new vfs::OverlayFileSystem(vfs::getRealFileSystem())
    );

    // Compute all absolute paths before we run any actions, as those will change
    // the working directory.
    AbsolutePaths.reserve(OptionsParser.getSourcePathList().size());
    for (const auto &SourcePath : OptionsParser.getSourcePathList()) {
      auto AbsPath = ct::getAbsolutePath(*OverlayFileSystem, SourcePath);
      if (!AbsPath) {
        errs() << "Skipping " << SourcePath
                    << ". Error while getting an absolute path: "
                    << toString(AbsPath.takeError()) << "\n";
        continue;
      }
      AbsolutePaths.push_back(move(*AbsPath));
    }

    // Scan for additional C files and directories to put in the include paths from a given root project
    for(string edir : InputDir) {
      scandir(*OverlayFileSystem, edir, IncludePath, AbsolutePaths);
    }

    // Add include paths
    for(auto I : IncludePath) {
      auto AbsPath = ct::getAbsolutePath(*OverlayFileSystem, I);
      if (!AbsPath) {
        errs() << "Skipping " << I
                    << ". Error while getting an absolute path: "
                    << toString(AbsPath.takeError()) << "\n";
        continue;
      }
      args.push_back("-I"+string(AbsPath->c_str()));
    }

    // Add def symbols
    for(auto D : Definitions) {
      args.push_back("-D"+D);
    }

      //Build an empty AST
    unique_ptr<ASTUnit> ast = ct::buildASTFromCode("", "empty.c");

    //Build all other ASTs and merge them into the empty one
    for (StringRef File : AbsolutePaths) {
      //get file content
      ifstream ifs(File.str());
      string content( (istreambuf_iterator<char>(ifs) ),
                        (istreambuf_iterator<char>()    ) );
      tooling::FileContentMappings files;
      files.push_back(make_pair(File.str(), content));

      //build ast
      unique_ptr<ASTUnit> tmp_ast = ct::buildASTFromCodeWithArgs(
          content, args, File.str(), "statinf-CFG-with-trace", PCHContainerOps,
          tooling::getClangStripDependencyFileAdjuster(), files
      );
      if(tmp_ast == nullptr) {
        errs() << "No AST have been built for " << File.str() << "\n";
        return -1;
      }

      //import each toplevel declaration one by one
      ASTImporter Importer(ast->getASTContext(), ast->getFileManager(),
                      tmp_ast->getASTContext(), tmp_ast->getFileManager(),
                      /*MinimalImport=*/false);
      for(auto decl : tmp_ast->getASTContext().getTranslationUnitDecl()->decls()) {
        auto ImportedOrErr = Importer.Import(decl);
        if (!ImportedOrErr) {
          Error Err = ImportedOrErr.takeError();
          errs() << "ERROR: " << Err << "\n";
          consumeError(move(Err));
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

    unique_ptr<raw_ostream> OutFile = nullptr;
    if(!Out.empty()) {
      error_code EC;
      OutFile = make_unique<raw_fd_ostream>(Out, EC);
      if (EC) {
        errs() << EC.message() << "\n";
        return -1;
      }
    }

    loop_detect.print(OutFile ? *(OutFile.get()) : outs());

    return 0;
}