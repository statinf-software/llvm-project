#include "CFGPrinter.h"
#include "llvm/ADT/PostOrderIterator.h"

using namespace clang;

namespace dot {

static std::string sanitize(StmtPrinterHelper &Helper, const std::string &input) {
  return Helper.str_replace(
          Helper.str_replace(
          Helper.str_replace(input, 
            "\"", "\'"), 
            "<", "\\<"), 
            ">", "\\>")
  ;
}

static bool print_elem(raw_ostream &OS, StmtPrinterHelper &Helper,
                       const CFGElement &E, bool &is_exec) {
  switch (E.getKind()) {
    case CFGElement::Kind::Statement: {
      CFGStmt CS = E.castAs<CFGStmt>();
      const Stmt *S = CS.getStmt();
      assert(S != nullptr && "Expecting non-null Stmt");

      if(S->getExecCount())
        is_exec = true;

      if(!Helper.acceptStmt(S))
        return false;
      
      S->printPretty(OS, nullptr, Helper.getPolicy(), 0, "");
      if(Helper.getPolicy().debug)
        OS << " (" << S->getStmtClassName() << ")";

      return true;
    }

    default:
      is_exec = true;
      break;
  }
  return false;
}

static void print_block(raw_ostream &OS, const CFG* cfg,
                        const CFGBlock &B,
                        StmtPrinterHelper &Helper, bool &is_bb_exec) {
  OS << "node_" << Helper.getCFGName() << B.getBlockID();
  OS << "[label=\"{BB" << B.getBlockID();


  // Iterate through the statements in the block and print them.
  std::string stmts_str;
  llvm::raw_string_ostream stmts_stream(stmts_str);
  unsigned j = 0;
  for (CFGBlock::const_iterator I = B.begin(), E = B.end() ; I != E ; ++I ) {
    std::string stmt_str;
    llvm::raw_string_ostream stmt_stream(stmt_str);
    stmt_stream << llvm::format("%3d", j) <<": ";
    bool is_exec = false;
    if(print_elem(stmt_stream, Helper, *I, is_exec)) {
      stmts_stream << sanitize(Helper, stmt_str) << "|";
      is_bb_exec = is_bb_exec || is_exec;
      ++j;
    }
  }
  stmts_str = stmts_str.substr(0, stmts_str.size()-1);
  if(j > 0) {
    Helper.addBB();
    if(is_bb_exec)
      Helper.addExecutedBB();
  }

  if(j > 2 && Helper.getPolicy().stmt_summary) {
    std::string tmp;
    tmp = stmts_str.substr(0, stmts_str.find_first_of('|'));
    tmp += " | ... ";
    tmp += stmts_str.substr(stmts_str.find_last_of('|'), stmts_str.size());
    stmts_str = tmp;
  }
  
  OS << stmts_str;

  OS << "}\"";
  if(j > 0 && Helper.getPolicy().add_coverage_color) {
    if(is_bb_exec)
      OS << ",color=\"green\"";
    else
      OS << ",color=\"red\"";
  }
  OS << "];\n";
}

static void print_edges(raw_ostream &OS, const CFG* cfg,
                        StmtPrinterHelper &Helper) {
  for (CFG::const_iterator I = cfg->nodes_begin(), E = cfg->nodes_end() ; I != E ; ++I) {
    const CFGBlock &B = **I;

  // Print the predecessors of this block.
    if (!B.pred_empty()) {
      unsigned i = 0;
      for (CFGBlock::const_pred_iterator I = B.pred_begin(), E = B.pred_end(); I != E; ++I, ++i) {
        CFGBlock *pred = *I;
        if (!pred) {
          pred = I->getPossiblyUnreachableBlock();
        }
        OS << "node_" << Helper.getCFGName() << pred->getBlockID() << " -> node_" << Helper.getCFGName() << B.getBlockID() << ";\n";
      }
    }
  }
}

static void printCFG(llvm::raw_ostream &Out, StmtPrinterHelper &Helper, const clang::StringRef name, CFG* cfg) {
  Helper.setCFGName(name);

  std::string cfg_str;
  llvm::raw_string_ostream strOS(cfg_str);

  FunctionDecl *fdecl = cfg->getInitialDecl()->getAsFunction();
  bool is_cfg_exec = (fdecl && fdecl->getEntryTimestamp().size());

  bool is_bb_exec = is_cfg_exec;
  // Print the entry block.
  Helper.addEntry(cfg->getEntry().getBlockID());
  print_block(strOS, cfg, cfg->getEntry(), Helper, is_bb_exec);

  // Iterate through the CFGBlocks and print them one by one.
  for (CFG::const_iterator I = cfg->nodes_begin(), E = cfg->nodes_end() ; I != E ; ++I) {
    // Skip the entry block, because we already printed it.
    if (&(**I) == &cfg->getEntry() || &(**I) == &cfg->getExit())
      continue;

    print_block(strOS, cfg, **I, Helper, is_bb_exec);
    is_cfg_exec = is_cfg_exec || is_bb_exec;
  }

  // Print the exit block.
  print_block(strOS, cfg, cfg->getExit(), Helper, is_bb_exec);

  print_edges(strOS, cfg, Helper);

  Out << "subgraph cluster_" << name << " {\n";
  Out << "\tlabel=\"" << name << "\";\n";
  Out << "\tfontsize=\"24\";\n";
  if(Helper.getPolicy().add_coverage_color) {
    if(is_cfg_exec) {
      Out << "color=\"green\"\n";
      Out << "node [color=\"green\"]\n";
    }
    else {
      Out << "color=\"red\"\n";
      Out << "node [color=\"red\"]\n";
    }
  }
  Out << cfg_str;
  Out << "}\n";
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

      cfgs_edges_stream << "node_" << pred_name << "0";
      cfgs_edges_stream << "->";
      cfgs_edges_stream << "node_" << succ_name << Helper.getEntryBBID(succ_name);
      cfgs_edges_stream << "[ltail=cluster_" << pred_name << ",lhead=cluster_" << succ_name << "];\n";
    }
  }

  Out << "strict digraph CFGs {\n";
  Out << "nodesep=0.5;\n";
  Out << "ranksep=1.2;\n";
  Out << "node [shape=record];\n";
  Out << "compound=true;\n";
  Out << "subgraph cluster_infos {\n";
  Out << "\tbgcolor=\"lightgrey\";\n";
  Out << "\tn1[label=\"Coverage: " << (Helper.getBBExecutedCount() / Helper.getBBcount()) * 100 << "%\",";
  Out << "color=\"lightgrey\",fontsize=\"42\",fontname=\"times-bold\"];\n";
  Out << "}\n";
  Out << cfgs_str;
  Out << cfgs_edges_str;
  Out << "}\n";
  Out.flush();
}
}