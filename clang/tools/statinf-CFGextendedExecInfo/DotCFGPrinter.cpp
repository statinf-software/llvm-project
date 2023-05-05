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

      // llvm::errs() << "(E="<<S->getExecCount()<<") ";
      // S->printPretty(llvm::errs(), nullptr, Helper.getPolicy(), 0, "");
      // llvm::errs()  << " (" << S->getStmtClassName() << ")\n";

      if(!Helper.acceptStmt(S))
        return false;

      if(S->getExecCount() > 0)
        is_exec = true;
      
      OS << "(E="<<S->getExecCount()<<")";
      S->printPretty(OS, nullptr, Helper.getPolicy(), 0, "");
      if(Helper.getPolicy().debug)
        OS << " (" << S->getStmtClassName() << ")";

      return true;
    }

    default:
      break;
  }
  return false;
}

static bool print_block(raw_ostream &OS, const CFG* cfg,
                        const CFGBlock &B,
                        StmtPrinterHelper &Helper) {
  bool is_bb_exec = false;
  OS << "node_" << Helper.getCFGName() << B.getBlockID();
  OS << "[label=\"{BB" << B.getBlockID();

  // llvm::errs() << "\tBB " << B.getBlockID() << "\n";

  // Iterate through the statements in the block and print them.
  std::string stmts_str;
  llvm::raw_string_ostream stmts_stream(stmts_str);
  unsigned j = 0;
  for (CFGBlock::const_iterator I = B.begin(), E = B.end() ; I != E ; ++I ) {
    std::string stmt_str;
    llvm::raw_string_ostream stmt_stream(stmt_str);
    stmt_stream << llvm::format("%3d", j) <<": ";
    // llvm::errs() << "\t\tStmt " << llvm::format("%3d", j) <<": ";
    bool is_exec = false;
    if(print_elem(stmt_stream, Helper, *I, is_exec)) {
      stmts_stream << sanitize(Helper, stmt_str) << "|";
      is_bb_exec = is_bb_exec || is_exec;
      ++j;
    }
    // llvm::errs() << "\t\t\texec " << is_exec << "\n";
  }
  // llvm::errs() << "\t\texec: " << is_bb_exec << "\n";
  stmts_str = stmts_str.substr(0, stmts_str.size()-1);

  if(j > 2 && Helper.getPolicy().stmt_summary) {
    std::string tmp;
    tmp = stmts_str.substr(0, stmts_str.find_first_of('|'));
    tmp += " | ... ";
    tmp += stmts_str.substr(stmts_str.find_last_of('|'), stmts_str.size());
    stmts_str = tmp;
  }
  
  OS << stmts_str;

  OS << "}\"";
  if(j > 0) {
    Helper.addBB();
    if(is_bb_exec)
      Helper.addBBExecuted(B.getBlockID());
  }
  else
    Helper.addBBSkipped(B.getBlockID());

  if(Helper.getPolicy().add_coverage_color) {
    if(Helper.isBBexecuted(B.getBlockID()))
      OS << ",color=\"green\"";
    else if(Helper.isBBSkipped(B.getBlockID()))
      OS << ",color=\"orange\""; // for automatically added BB that have no instructions
    else
      OS << ",color=\"red\"";
  }
  OS << "];\n";


  return is_bb_exec;
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
        OS << "node_" << Helper.getCFGName() << pred->getBlockID() << " -> node_" << Helper.getCFGName() << B.getBlockID();

        if(Helper.getPolicy().add_coverage_color) {
          if(Helper.isBBexecuted(pred->getBlockID()) && Helper.isBBexecuted(B.getBlockID()))
            OS << "[color=\"green\"]";
          else if(Helper.isBBSkipped(pred->getBlockID()) || Helper.isBBSkipped(B.getBlockID()))
            OS << "[color=\"orange\"]";
          else
            OS << "[color=\"red\"]";
        }
        
        OS << ";\n";
      }
    }
  }
}

static void printCFG(llvm::raw_ostream &Out, StmtPrinterHelper &Helper, const clang::StringRef name, CFG* cfg) {
  Helper.setCFGName(name);
  // llvm::errs() << name << "\n";

  std::string cfg_str;
  llvm::raw_string_ostream strOS(cfg_str);

  FunctionDecl *fdecl = cfg->getInitialDecl()->getAsFunction();
  bool is_cfg_exec = (fdecl && fdecl->getEntryTimestamp().size());
  // llvm::errs() << "\tinit exec: " << is_cfg_exec << "\n";

  // Print the entry block.
  Helper.addEntry(cfg->getEntry().getBlockID());
  bool is_bb_exec = print_block(strOS, cfg, cfg->getEntry(), Helper);
  is_cfg_exec = is_cfg_exec || is_bb_exec;

  // Iterate through the CFGBlocks and print them one by one.
  for (CFG::const_iterator I = cfg->nodes_begin(), E = cfg->nodes_end() ; I != E ; ++I) {
    // Skip the entry block, because we already printed it.
    if (&(**I) == &cfg->getEntry() || &(**I) == &cfg->getExit())
      continue;

    bool is_bb_exec = print_block(strOS, cfg, **I, Helper);
    is_cfg_exec = is_cfg_exec || is_bb_exec;
  }

  // Print the exit block.
  is_bb_exec = print_block(strOS, cfg, cfg->getExit(), Helper);
  is_cfg_exec = is_cfg_exec || is_bb_exec;

  if(is_cfg_exec) 
    Helper.addCFGExecuted();

  print_edges(strOS, cfg, Helper);

  Out << "subgraph cluster_" << name << " {\n";
  Out << "\tlabel=\"" << name << "\";\n";
  Out << "\tfontsize=\"24\";\n";
  if(Helper.getPolicy().add_coverage_color) {
    if(Helper.isCFGExecuted())
      Out << "color=\"green\"\n";
    else if(Helper.isCFGUnknown())
      Out << "color=\"orange\"\n";
    else
      Out << "color=\"red\"\n";
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

  std::map<std::string, bool> unknown_cfg;
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

    if(Helper.isCFGUnknown(pred_name))
        unknown_cfg[pred_name] = true;

    for (CallGraphNode::const_iterator CI = N->begin(),
                                       CE = N->end(); CI != CE; ++CI) {
      std::string succ_name;
      llvm::raw_string_ostream sname_s(succ_name);
      CI->Callee->print(sname_s);
      if(Helper.isCFGUnknown(succ_name))
        unknown_cfg[succ_name] = true;

      cfgs_edges_stream << "node_" << pred_name << (Helper.isCFGUnknown(pred_name) ? "_empty" : "0");
      cfgs_edges_stream << "->";
      cfgs_edges_stream << "node_" << succ_name << (Helper.isCFGUnknown(succ_name) ? "_empty" : std::to_string(Helper.getEntryBBID(succ_name)));
      cfgs_edges_stream << "[ltail=cluster_" << pred_name << ",lhead=cluster_" << succ_name;

      if(Helper.getPolicy().add_coverage_color) {
        if(Helper.isCFGExecuted(pred_name) && Helper.isCFGExecuted(succ_name))
          cfgs_edges_stream << ",color=\"green\"";
        else if(Helper.isCFGUnknown(pred_name) || Helper.isCFGUnknown(succ_name))
          cfgs_edges_stream << ",color=\"orange\"";
        else
          cfgs_edges_stream << ",color=\"red\"";
      }

      cfgs_edges_stream << "];\n";
    }
  }

  //Functions without body
  for(auto el : unknown_cfg) {
    cfgs_stream << "subgraph cluster_" << el.first << " {\n";
    cfgs_stream << "\tlabel=\"" << el.first << "\";\n";
    cfgs_stream << "\tfontsize=\"24\";\n";
    if(Helper.getPolicy().add_coverage_color)
      cfgs_stream << "\tcolor=\"orange\";\n";
    cfgs_stream << "\tnode_"<<el.first<<"_empty[label=\"Missing function body\",style=filled,fillcolor=\"red\",color=\"white\"];\n";
    cfgs_stream << "}\n";
  }

  Out << "strict digraph CFGs {\n";
  Out << "nodesep=0.5;\n";
  Out << "ranksep=1.2;\n";
  Out << "node [shape=record];\n";
  Out << "compound=true;\n";
  Out << "subgraph cluster_infos {\n";
  Out << "\tbgcolor=\"lightgrey\";\n";
  Out << "\tn1[label=\"Coverage: " << llvm::format("%4.2f", Helper.getCoverage()) << "%\",";
  Out << "color=\"lightgrey\",fontsize=\"42\",fontname=\"times-bold\"];\n";
  Out << "}\n";
  Out << cfgs_str;
  Out << cfgs_edges_str;
  Out << "}\n";
  Out.flush();
  llvm::outs() << "BB count: " << Helper.getBBcount() << "\n";
  llvm::outs() << "Executed BB count: " << Helper.getBBExecutedCount() << "\n";
  llvm::outs() << "Coverage: " << llvm::format("%4.2f", Helper.getCoverage()) << "%\n";
}
}