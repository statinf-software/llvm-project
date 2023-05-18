//===--- tools/clang-check/ClangCheck.cpp - Clang check tool --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements a clang-check tool that runs clang based on the info
//  stored in a compilation database.
//
//  This tool uses the Clang Tooling infrastructure, see
//    http://clang.llvm.org/docs/HowToSetupToolingForLLVM.html
//  for details on setting it up with LLVM source tree.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/CodeGen/ObjectFilePCHContainerOperations.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "clang/Rewrite/Frontend/FrontendActions.h"
#include "clang/StaticAnalyzer/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Syntax/BuildTree.h"
#include "clang/Tooling/Syntax/TokenBufferTokenManager.h"
#include "clang/Tooling/Syntax/Tokens.h"
#include "clang/Tooling/Syntax/Tree.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Casting.h"
#include "clang/Analysis/CFG.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

#include "clang/AST/StatInfASTExtendExecInfoDecl.h"
#include "clang/AST/StatInfASTResetExec.h"

#include "CFGPrinter.h"
#include "../StatInfCommon/StatInfUtils.h"


#include <vector>
#include <string>
#include <fstream>
#include <iostream>

using namespace llvm;
using namespace clang;
using namespace std;
namespace cd = clang::driver;
namespace ct = clang::tooling;
namespace am = clang::ast_matchers;
using MatchResult = am::MatchFinder::MatchResult;
using MatchCallback = am::MatchFinder::MatchCallback;
using ListCFGs = map<StringRef, unique_ptr<CFG>>;

static cl::extrahelp CommonHelp(ct::CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp(
   "\tCombine a trace bitstream file into the inter-procedural CFG of an application\n"
   "\t===> WARNING: the provided C files must not contain instrumentation code <===\n"
   "\tThe bitstream file can contain the information for a temporal or structural analysis or both\n"
   "\t\tit is required to specify with analysis is targeted\n"
   "Options:\n"
);

static cl::OptionCategory CFGExtendExecInfoCat("clang-check options");
static const opt::OptTable &Options = cd::getDriverOptTable();
static cl::opt<string>
    EntryPoint(
        "entrypoint",
        cl::desc("Entrypoint function name of the beginning of the trace -- default main"),
        cl::cat(CFGExtendExecInfoCat)
    );
static cl::opt<string>
  BitstreamFile("trace",
    cl::desc("Trace file containing the bitstream of an execution (full path)"),
    cl::cat(CFGExtendExecInfoCat)
  );
static cl::opt<bool>
    IsStructuralAnalysis("structural",
      cl::desc("The provided trace is contains structural execution information (if-then-else, loops, ...)"),
      cl::cat(CFGExtendExecInfoCat)
    );
static cl::opt<bool>
    IsTemporalAnalysis("temporal",
      cl::desc("The provided trace is contains temporal execution information (uniquement entrypoint)"),
      cl::cat(CFGExtendExecInfoCat)
    );
static cl::opt<size_t>
    TimeBitsSize("time-num-bits",
      cl::desc("Number of bits to represent a timestamp for the temporal analysis -- default 32 bits"),
      cl::cat(CFGExtendExecInfoCat)
    );
static cl::opt<bool>
  OutputDot("dot",
    cl::desc("Enable output in dot format and store it in the given file, dot extension will automatically be added"),
    cl::cat(CFGExtendExecInfoCat)
  );
static cl::opt<bool>
  OutputJson("json",
    cl::desc("Enable output in json format and store it in the given file, json extension will automatically be added"),
    cl::cat(CFGExtendExecInfoCat)
  );
static cl::list<string>
    IncludePath("I", cl::desc(Options.getOptionHelpText(cd::options::OPT_include)),
                  cl::cat(CFGExtendExecInfoCat));
static cl::list<string>
    Definitions("D", cl::desc(Options.getOptionHelpText(cd::options::OPT_defsym)),
                  cl::cat(CFGExtendExecInfoCat));
static cl::opt<string>
    InputDir("input-dir", 
      cl::desc("Recursively scans this directory to find all .c files, also add all found directories in the include path"),
      cl::cat(CFGExtendExecInfoCat)
    );
static cl::opt<string>
    Out(
      "o",
      cl::desc("File in which storing the output !! don't add a file extension !!"),
      cl::cat(CFGExtendExecInfoCat)
    );
static cl::opt<bool>
    Debug(
      "d",
      cl::desc("Debug mode, add extra info in the CFG"),
      cl::cat(CFGExtendExecInfoCat)
    );
static cl::opt<string>
    SplitDir(
      "split-dir",
      cl::desc("Enable to generate a separate CFG dot file per execution got from the bitstream. "
              "This options specifies in which folder the generated files are stored."),
      cl::cat(CFGExtendExecInfoCat)
    );
static cl::opt<bool>
    FullInstrInBB(
      "full-instr",
      cl::desc("List all instructions in a BB, if disable (default) only prints a summary (1st and last)"),
      cl::cat(CFGExtendExecInfoCat)
    );
static cl::opt<string>
    Filter("filter",
      cl::desc("Filter out files that matches the given pattern, the full path is checked so a full directory can been filter out."),
      cl::cat(CFGExtendExecInfoCat)
    );

namespace {

static void exportCFGs(const string &output_file, ASTContext &ctx, ListCFGs &CFGs, CallGraph *cg) {
  CFGPrinterPolicy policy(ctx.getLangOpts());
  policy.debug = Debug;
  policy.add_coverage_color = true;
  policy.stmt_summary = !FullInstrInBB;
  if(OutputDot) {
    unique_ptr<raw_ostream> OutFile = nullptr;
    if(!output_file.empty()) {
      error_code EC;
      OutFile = make_unique<raw_fd_ostream>(output_file+".dot", EC);
      if (EC) {
        errs() << EC.message() << "\n";
        return;
      }
    }
    dot::printCFG(OutFile ? *(OutFile.get()) : outs(), CFGs, cg, policy);
  }
  
  if(OutputJson) {
    unique_ptr<raw_ostream> OutFile = nullptr;
    if(!output_file.empty()) {
      error_code EC;
      OutFile = make_unique<raw_fd_ostream>(output_file+".json", EC);
      if (EC) {
        errs() << EC.message() << "\n";
        return;
      }
    }
    json::printCFG(OutFile ? *(OutFile.get()) : outs(), CFGs, cg, policy);
  }
}

class CFGCallback : public MatchCallback {
public:
  CFGCallback(unique_ptr<ASTUnit> AST) : AST(move(AST)) {}

  unique_ptr<ASTUnit> AST;
  CFG::BuildOptions Options;
  ListCFGs CFGs;

  void run(const MatchResult &Result) override {
    const auto *Func = Result.Nodes.getNodeAs<FunctionDecl>("func");
    Stmt *Body = Func->getBody();
    if (!Body)
      return;
    Options.AddImplicitDtors = true;
    unique_ptr<CFG> cfg = CFG::buildCFG(Func, Body, Result.Context, Options);
    if(cfg) {
      CFGs[Func->getName()] = move(cfg);
    }
  }
};

class ExtendExecInfoFull : public MatchCallback {
  const vector<unsigned char> &bitstream_trace;
  map<StringRef, unique_ptr<CFG>> &CFGs;
  CallGraph *cg;
public:
  ExtendExecInfoFull(const vector<unsigned char> &bt,
  map<StringRef, unique_ptr<CFG>> &_CFGs, CallGraph *_cg) : 
    MatchCallback(), bitstream_trace(bt), CFGs(_CFGs), cg(_cg) {}

  ~ExtendExecInfoFull() {}

  virtual void run(const MatchResult &Result) override {
    if (const FunctionDecl *FS = Result.Nodes.getNodeAs<FunctionDecl>("entrypoint")) {
      errs() << "Add trace info " << FS->getName() << "\n";
      StatInfASTExtendExecInfoDecl extend(FS->getASTContext(), FS->getName(), bitstream_trace, IsStructuralAnalysis, IsTemporalAnalysis, TimeBitsSize);
      while(!extend.EOBS()) {
        extend.VisitFunctionDecl(const_cast<FunctionDecl*>(FS));
      }
      for(auto mb : extend.getMissingFunctionBody())
        errs() << "Missing function body: " << mb << "\n";
    }
  }
};

class ExtendExecInfoSplitExec : public MatchCallback {
  const vector<unsigned char> &bitstream_trace;
  map<StringRef, unique_ptr<CFG>> &CFGs;
  CallGraph *cg;
public:
  ExtendExecInfoSplitExec(const vector<unsigned char> &bt,
  map<StringRef, unique_ptr<CFG>> &_CFGs, CallGraph *_cg) : 
    MatchCallback(), bitstream_trace(bt), CFGs(_CFGs), cg(_cg) {}

  ~ExtendExecInfoSplitExec() {}

  virtual void run(const MatchResult &Result) override {
    if (const FunctionDecl *FS = Result.Nodes.getNodeAs<FunctionDecl>("entrypoint")) {
      errs() << "Add trace info " << FS->getName() << "\n";
      StatInfASTExtendExecInfoDecl extend(FS->getASTContext(), FS->getName(), bitstream_trace, IsStructuralAnalysis, IsTemporalAnalysis, TimeBitsSize);
      StatInfASTResetExecInfoDecl reset(FS->getASTContext());
      size_t exec_count = 0;
      while(!extend.EOBS()) {
        ++exec_count;
        extend.VisitFunctionDecl(const_cast<FunctionDecl*>(FS));
        exportCFGs(SplitDir+"/exec_"+to_string(exec_count), FS->getASTContext(), CFGs, cg);
        reset.VisitFunctionDecl(const_cast<FunctionDecl*>(FS));
      }
      for(auto mb : extend.getMissingFunctionBody())
        errs() << "Missing function body: " << mb << "\n";
    }
  }
};

static unique_ptr<ASTUnit> extractCFG(unique_ptr<ASTUnit> ast, ListCFGs &CFGs) {
  // Configure the CFG builder
  CFG::BuildOptions Options;
  Options.AddStaticInitBranches = true;
  Options.setAllAlwaysAdd();
  Options.AddLoopExit = true;
  Options.AddExecutionStateExtractedFromATrace = true;

  // Build the CFG
  CFGCallback Callback(move(ast));
  Callback.Options = Options;
  am::MatchFinder Finder2;
  Finder2.addMatcher(am::functionDecl(am::anything()).bind("func"), &Callback);
  Finder2.matchAST(Callback.AST->getASTContext());

  for(auto &I : Callback.CFGs) {
    CFGs[I.first] = move(I.second);
  }
  
  return move(Callback.AST);
}

} // namespace

int main(int argc, const char **argv) {
  sys::PrintStackTraceOnErrorSignal(argv[0]);

  EntryPoint.setInitialValue("main");
  TimeBitsSize.setInitialValue(32);

  auto ExpectedParser =
      ct::CommonOptionsParser::create(argc, argv, CFGExtendExecInfoCat, cl::NumOccurrencesFlag::ZeroOrMore);
  if (!ExpectedParser) {
    errs() << ExpectedParser.takeError();
    return 1;
  }

  if(!IsStructuralAnalysis && !IsTemporalAnalysis) {
    errs() << "Please specify at least one of --structural or --temporal parameters\n";
    errs() << ExpectedParser.takeError();
    return 1;
  }

  if(!OutputDot && !OutputJson) {
    errs() << "Please specify at least one of --dot or --json parameters\n";
    errs() << ExpectedParser.takeError();
    return 1;
  }

  ct::CommonOptionsParser &OptionsParser = ExpectedParser.get();

  // Get the content of the bitstream trace file
  vector<uint8_t> *bitstream_trace = nullptr;
  if(!BitstreamFile.empty()) {
    ifstream bitstream_fs(BitstreamFile, ios::binary);
    if(!bitstream_fs.is_open()) {
      errs() << "Error opening " << BitstreamFile << "\n";
      return 1;
    }
    bitstream_trace = new vector<uint8_t>(istreambuf_iterator<char>(bitstream_fs), {});
    bitstream_fs.close();
  }

  shared_ptr<PCHContainerOperations> PCHContainerOps = make_shared<PCHContainerOperations>();
  vector<string> C_files, other_files;
  IntrusiveRefCntPtr<vfs::OverlayFileSystem> OverlayFileSystem(
    new vfs::OverlayFileSystem(vfs::getRealFileSystem())
  );

  get_c_files_from_cmdline(C_files, *OverlayFileSystem, OptionsParser);

  if(!InputDir.empty() && !C_files.empty()) {
    errs() << "Can't provide a list of files and an input-dir to scan for source files.\n";
    return 1;
  }

  // Scan for additional C files and directories to put in the include paths from a given root project
  scandir(*OverlayFileSystem, InputDir, IncludePath, C_files, other_files, Filter);

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

  ListCFGs CFGs;
  ast = extractCFG(move(ast), CFGs);
  CallGraph cg;
  extractCallGraph(ast.get(), &cg, EntryPoint);

  // Add the execution information from the trace
  if(bitstream_trace) {
    am::DeclarationMatcher entrypoint_match = am::functionDecl(am::hasName(EntryPoint)).bind("entrypoint");

    ExtendExecInfoFull extendor(*bitstream_trace, CFGs, &cg);
    am::MatchFinder Finder;
    Finder.addMatcher(entrypoint_match, &extendor);

    ExtendExecInfoSplitExec split_extend(*bitstream_trace, CFGs, &cg);
    if(!SplitDir.empty()) {
      Finder.addMatcher(entrypoint_match, &split_extend);
    }

    Finder.matchAST(ast->getASTContext());
    delete bitstream_trace;
  }

  exportCFGs(Out, ast->getASTContext(), CFGs, &cg);
  
  return 0;
}
