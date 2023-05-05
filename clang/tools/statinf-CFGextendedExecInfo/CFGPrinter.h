#include "clang/Analysis/CFG.h"
#include "llvm/Support/Format.h"
#include "clang/AST/StmtVisitor.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Analysis/CallGraph.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/Stmt.h"

struct CFGPrinterPolicy : public clang::PrintingPolicy {
    CFGPrinterPolicy(const clang::LangOptions &LO) : clang::PrintingPolicy(LO) {}

    bool debug = false; //Add some debug info in the CFG (StmtClass)
    bool stmt_summary = false; //Don't print all the instructions, only first and last of a BB
    bool add_coverage_color = true; //Add color for executed/non-executed BB/CFG
    bool cfg_in_callgraph_only = true; //Print only CFGs that are in the callgraph of the entrypoint
};

class StmtPrinterHelper  {
  CFGPrinterPolicy &Policy;
  llvm::StringRef cfg_name;
  llvm::DenseMap<llvm::StringRef, unsigned> entries;
  size_t BB_count=0;
  size_t BB_executed_count=0;
  std::map<llvm::StringRef, bool> is_cfg_executed;
  std::map<llvm::StringRef, bool> cfgs;
  std::map<llvm::StringRef, std::map<unsigned, bool>> is_BB_executed;
  std::map<llvm::StringRef, std::map<unsigned, bool>> is_BB_skipped;

public:
  StmtPrinterHelper(CFGPrinterPolicy &pol) : Policy(pol) {}

  CFGPrinterPolicy &getPolicy() const { return Policy; }
  void setCFGName(const llvm::StringRef name) { cfgs[name]; cfg_name = name; }
  llvm::StringRef getCFGName() { return cfg_name; }

  void addCFGExecuted() {is_cfg_executed[cfg_name] = true;}
  bool isCFGExecuted() { return is_cfg_executed.count(cfg_name) && is_cfg_executed[cfg_name];}
  bool isCFGExecuted(const std::string &cname) { return is_cfg_executed.count(cname) && is_cfg_executed[cname];}

  bool isCFGUnknown() { return !cfgs.count(cfg_name);}
  bool isCFGUnknown(const std::string &cname) { return !cfgs.count(cname);}

  void addEntry(unsigned bbid) {
    entries[cfg_name] = bbid;
  }

  unsigned getEntryBBID(llvm::StringRef cname) {
    if(entries.count(cname))
      return entries[cname];
    return 0;
  }
  void addBB() {++BB_count;}
  void addBBExecuted(unsigned bbid) {is_BB_executed[cfg_name][bbid] = true; ++BB_executed_count;}
  bool isBBexecuted(unsigned bbid) { return is_BB_executed[cfg_name].count(bbid) && is_BB_executed[cfg_name][bbid];}

  void addBBSkipped(unsigned bbid) {is_BB_skipped[cfg_name][bbid] = true;}
  bool isBBSkipped(unsigned bbid) { return is_BB_skipped[cfg_name].count(bbid) && is_BB_skipped[cfg_name][bbid];}

  size_t getBBcount() {return BB_count;}
  size_t getBBExecutedCount() {return BB_executed_count;}
  float getCoverage() { return (BB_executed_count / (float)BB_count) * 100;}

  std::string str_replace(const std::string &ci, const std::string &search, const std::string &replace) {
    size_t pos=0;
    std::string input(ci);
    while((pos = input.find(search, pos)) != std::string::npos) {
        input.replace(pos, search.length(), replace);
        pos += replace.length();
    }
    return input;
  }

  bool acceptStmt(const clang::Stmt *S) {
    switch(S->getStmtClass()) {
        case clang::Stmt::StmtClass::DeclRefExprClass:
        case clang::Stmt::StmtClass::ImplicitCastExprClass:
        case clang::Stmt::StmtClass::CStyleCastExprClass:
        case clang::Stmt::StmtClass::IntegerLiteralClass:
        case clang::Stmt::StmtClass::FixedPointLiteralClass:
        case clang::Stmt::StmtClass::FloatingLiteralClass:
        case clang::Stmt::StmtClass::ImaginaryLiteralClass:
        case clang::Stmt::StmtClass::StringLiteralClass:
        case clang::Stmt::StmtClass::CharacterLiteralClass:
        case clang::Stmt::StmtClass::CompoundLiteralExprClass:
        case clang::Stmt::StmtClass::UserDefinedLiteralClass:
        case clang::Stmt::StmtClass::CXXBoolLiteralExprClass:
        case clang::Stmt::StmtClass::CXXNullPtrLiteralExprClass:
        case clang::Stmt::StmtClass::ObjCStringLiteralClass:
        case clang::Stmt::StmtClass::ObjCArrayLiteralClass:
        case clang::Stmt::StmtClass::ObjCDictionaryLiteralClass:
        case clang::Stmt::StmtClass::ObjCBoolLiteralExprClass:
          return false;
        default:
          return true;
      }
  } 
};

namespace dot {
void printCFG(llvm::raw_ostream &OS,  
    std::map<clang::StringRef, std::unique_ptr<clang::CFG>> &CFGs, 
    clang::CallGraph *cg, 
    CFGPrinterPolicy &LO);
}

namespace json {
void printCFG(llvm::raw_ostream &OS,  
    std::map<clang::StringRef, std::unique_ptr<clang::CFG>> &CFGs, 
    clang::CallGraph *cg, 
    CFGPrinterPolicy &LO);
}