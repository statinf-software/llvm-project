#include "CFGPrinter.h"
#include "llvm/ADT/PostOrderIterator.h"

using namespace clang;

namespace json {

static std::string sanitize(StmtPrinterHelper &Helper, const std::string &input) {
  return Helper.str_replace(input, 
            "\"", "\'")
  ;
}

static uint32_t print_elem(raw_ostream &OS, StmtPrinterHelper &Helper, const CFGElement &E) {
  switch (E.getKind()) {
    case CFGElement::Kind::Statement: {
      CFGStmt CS = E.castAs<CFGStmt>();
      const Stmt *S = CS.getStmt();
      assert(S != nullptr && "Expecting non-null Stmt");

      if(!Helper.acceptStmt(S))
        return 0;
        
      OS << "\t\t\t\t\t\t\"";
      std::string stmt_str;
      llvm::raw_string_ostream stmt_stream(stmt_str);
      S->printPretty(stmt_stream, nullptr, Helper.getPolicy(), 0, "");
      OS << sanitize(Helper, stmt_str);
      if(Helper.getPolicy().debug)
        OS << " (" << S->getStmtClassName() << ")";
      OS << "\",\n";
      return S->getExecCount();
    }

    default:
      break;
  }
  return 0;
}

static void print_block(raw_ostream &OS, const CFG* cfg,
                        const CFGBlock &B,
                        StmtPrinterHelper &Helper) {

  uint32_t bb_count = 0;
  std::string stmts_str;
  llvm::raw_string_ostream stmts_stream(stmts_str);

  unsigned j = 0;
  for (CFGBlock::const_iterator I = B.begin(), E = B.end() ; I != E ; ++I ) {
    std::string stmt_str;
    llvm::raw_string_ostream stmt_stream(stmt_str);
    uint32_t count = print_elem(stmt_stream, Helper, *I);
    if(count) {
      bb_count = count;
      stmts_stream << stmt_str << "|";
      ++j;
    }
  }
  stmts_str = stmts_str.substr(0, stmts_str.size()-1);
  if(j > 0) {
    Helper.addBB();
    if(bb_count > 0)
      Helper.addExecutedBB();
  }

  if(j > 2 && Helper.getPolicy().stmt_summary) {
    std::string tmp;
    tmp = stmts_str.substr(0, stmts_str.find_first_of('|'));
    tmp += stmts_str.substr(stmts_str.find_last_of('|'), stmts_str.size());
    stmts_str = tmp;
  }
  stmts_str = Helper.str_replace(stmts_str, "|", "");

  OS << "\t\t\t\t\"BB" << B.getBlockID() << "\": {\n";
  OS << "\t\t\t\t\t\"exec_count\": " << bb_count << ",\n";
  OS << "\t\t\t\t\t\"stmts\": [\n";
  OS << stmts_str.substr(0, stmts_str.size()-2) << "\n";
  OS << "\t\t\t\t\t]\n";
  OS << "\t\t\t\t},\n";
}

static void print_edges(raw_ostream &OS, const CFG* cfg,
                        StmtPrinterHelper &Helper) {
  for (CFG::const_iterator I = cfg->nodes_begin(), E = cfg->nodes_end() ; I != E ; ++I) {
    const CFGBlock &B = **I;

  // Print the predecessors of this block.
    if (!B.pred_empty()) {
      for (CFGBlock::const_pred_iterator I = B.pred_begin(), E = B.pred_end(); I != E; ++I) {
        CFGBlock *pred = *I;
        if (!pred)
          pred = I->getPossiblyUnreachableBlock();
        OS << "\t\t\t\t[\"BB" << pred->getBlockID() << "\", \"BB" << B.getBlockID() << "\"],\n";
      }
    }
  }
}

static void printCFG(llvm::raw_ostream &Out, StmtPrinterHelper &Helper, const clang::StringRef name, CFG* cfg) {
  FunctionDecl *fdecl = cfg->getInitialDecl()->getAsFunction();

  // Iterate through the CFGBlocks and print them one by one.
  std::string blocks_str;
  llvm::raw_string_ostream blocks_stream(blocks_str);
  for (CFG::const_iterator I = cfg->nodes_begin(), E = cfg->nodes_end() ; I != E ; ++I) {
    print_block(blocks_stream, cfg, **I, Helper);
  }

  std::string edges_str;
  llvm::raw_string_ostream edges_stream(edges_str);
  print_edges(edges_stream, cfg, Helper);

  std::string entry_str;
  llvm::raw_string_ostream entry_stream(entry_str);
  for(auto t : fdecl->getEntryTimestamp())
    entry_stream << "\t\t\t\t" << t << ",";
  entry_str = entry_str.substr(0, entry_str.size()-1);

  std::string exit_str;
  llvm::raw_string_ostream exit_stream(exit_str);
  for(auto t : fdecl->getExitTimestamp())
    exit_stream << "\t\t\t\t" << t << ",";
  exit_str = exit_str.substr(0, exit_str.size()-1);

  Out << "\t\t\"" << name << "\": {\n";
  Out << "\t\t\t\"entry_timestamp\": [\n";
  Out << entry_str << "\n";
  Out << "\t\t\t],\n";
  Out << "\t\t\t\"exit_timestamp\": [\n";
  Out << exit_str << "\n";
  Out << "\t\t\t],\n";

  Out << "\t\t\t\"nodes\": {\n";
  Out << blocks_str.substr(0, blocks_str.size()-2) << "\n";
  Out << "\t\t\t},\n";

  Out << "\t\t\t\"edges\": {\n";
  Out << edges_str.substr(0, edges_str.size()-2) << "\n";
  Out << "\t\t\t}\n";
  Out << "\t\t},\n";
}

void printCFG(llvm::raw_ostream &Out,  
  std::map<clang::StringRef, std::unique_ptr<clang::CFG>> &CFGs, 
  clang::CallGraph *cg, 
  CFGPrinterPolicy &Policy) {

  StmtPrinterHelper Helper(Policy);

  std::string cfgs_str;
  llvm::raw_string_ostream cfgs_stream(cfgs_str);
  for(std::pair<const clang::StringRef, std::unique_ptr<clang::CFG>> &el : CFGs) {
    if(!Helper.getPolicy().cfg_in_callgraph_only || cg->getNode(el.first))
      printCFG(cfgs_stream, Helper, el.first, el.second.get());
  }
  
  std::string cfgs_edges_str;
  llvm::raw_string_ostream cfgs_edges_stream(cfgs_edges_str);
  llvm::ReversePostOrderTraversal<const CallGraph *> RPOT(cg);
  for (llvm::ReversePostOrderTraversal<const CallGraph *>::rpo_iterator
         I = RPOT.begin(), E = RPOT.end(); I != E; ++I) {
    const CallGraphNode *N = *I;

    if(N == cg->getRoot())
      continue;

    std::string pred_name;
    llvm::raw_string_ostream pname_s(pred_name);
    N->print(pname_s);

    for (CallGraphNode::const_iterator CI = N->begin(),
                                       CE = N->end(); CI != CE; ++CI) {
      std::string succ_name;
      llvm::raw_string_ostream sname_s(succ_name);
      CI->Callee->print(sname_s);

      cfgs_edges_stream << "[\"" << pred_name << "\", \"" << succ_name << "\"],\n";
    }
  }

  Out << "{\n";
  Out << "\t\"coverage\": \"" << (Helper.getBBExecutedCount() / Helper.getBBcount()) * 100 << "%,\n";
  Out << "\t\"cfgs\": [\n";
  Out << cfgs_str.substr(0, cfgs_str.size()-2) << "\n";
  Out << "\t],\n";
  Out << "\t\"cfg_edges\": [";
  Out << cfgs_edges_str.substr(0, cfgs_edges_str.size()-2) << "\n";
  Out << "\t]\n";
  Out << "}\n";
  Out.flush();
}
}