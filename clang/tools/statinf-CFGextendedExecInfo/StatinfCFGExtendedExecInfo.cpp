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
#include "clang/AST/ASTImporter.h"
#include "clang/Analysis/CallGraph.h"

#include "clang/AST/StatInfASTExtendExecInfoDecl.h"
#include "clang/AST/StatInfASTResetExec.h"

#include "CFGPrinter.h"


#include <vector>
#include <string>
#include <fstream>
#include <iostream>

using namespace clang::driver;
using namespace clang::tooling;
using namespace llvm;
using namespace std;

namespace am = clang::ast_matchers;
using MatchResult = am::MatchFinder::MatchResult;
using MatchCallback = am::MatchFinder::MatchCallback;
using ListCFGs = map<llvm::StringRef, unique_ptr<clang::CFG>>;

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp(
   "\tCombine a trace bitstream file into the inter-procedural CFG of an application\n"
   "\t===> WARNING: the provided C files must not contain instrumentation code <===\n"
   "\tThe bitstream file can contain the information for a temporal or structural analysis or both\n"
   "\t\tit is required to specify with analysis is targeted\n"
   "Options:\n"
);

static cl::OptionCategory CFGExtendExecInfoCat("clang-check options");
static const opt::OptTable &Options = getDriverOptTable();
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
    IncludePath("I", cl::desc(Options.getOptionHelpText(options::OPT_include)),
                  cl::cat(CFGExtendExecInfoCat));
static cl::list<string>
    Definitions("D", cl::desc(Options.getOptionHelpText(options::OPT_defsym)),
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

namespace {

static void scandir(llvm::vfs::FileSystem &fs, StringRef dirname, cl::list<string> &dirs, vector<string> &C_files) {
  dirs.push_back(*(getAbsolutePath(fs, dirname)));
  error_code EC;
  for(llvm::vfs::directory_iterator elt = fs.dir_begin(dirname, EC), dirend ; elt != dirend && !EC; elt.increment(EC)) {
    if (elt->path().endswith(".c"))
      C_files.push_back(*(getAbsolutePath(fs, elt->path())));
    else if(elt->type() == llvm::sys::fs::file_type::directory_file)
      scandir(fs, elt->path(), dirs, C_files);
  }
}

static void exportCFGs(const string &output_file, clang::ASTContext &ctx, ListCFGs &CFGs, clang::CallGraph *cg) {
  CFGPrinterPolicy policy(ctx.getLangOpts());
  policy.debug = Debug;
  policy.add_coverage_color = true;
  policy.stmt_summary = !FullInstrInBB;
  if(OutputDot) {
    unique_ptr<llvm::raw_ostream> OutFile = nullptr;
    if(!output_file.empty()) {
      error_code EC;
      OutFile = make_unique<llvm::raw_fd_ostream>(output_file+".dot", EC);
      if (EC) {
        llvm::errs() << EC.message() << "\n";
        return;
      }
    }
    dot::printCFG(OutFile ? *(OutFile.get()) : llvm::outs(), CFGs, cg, policy);
  }
  
  if(OutputJson) {
    unique_ptr<llvm::raw_ostream> OutFile = nullptr;
    if(!output_file.empty()) {
      error_code EC;
      OutFile = make_unique<llvm::raw_fd_ostream>(output_file+".json", EC);
      if (EC) {
        llvm::errs() << EC.message() << "\n";
        return;
      }
    }
    json::printCFG(OutFile ? *(OutFile.get()) : llvm::outs(), CFGs, cg, policy);
  }
}

class CFGCallback : public MatchCallback {
public:
  CFGCallback(unique_ptr<clang::ASTUnit> AST) : AST(move(AST)) {}

  unique_ptr<clang::ASTUnit> AST;
  clang::CFG::BuildOptions Options;
  ListCFGs CFGs;

  void run(const MatchResult &Result) override {
    const auto *Func = Result.Nodes.getNodeAs<clang::FunctionDecl>("func");
    clang::Stmt *Body = Func->getBody();
    if (!Body)
      return;
    Options.AddImplicitDtors = true;
    unique_ptr<clang::CFG> cfg = clang::CFG::buildCFG(Func, Body, Result.Context, Options);
    if(cfg) {
      CFGs[Func->getName()] = move(cfg);
    }
  }
};

class CallGraphExtract : public MatchCallback {
  clang::CallGraph *callgraph;
public:
  explicit CallGraphExtract(clang::CallGraph *cg) : callgraph(cg) {}

  virtual void run(const MatchResult &Result) {
    if (const clang::FunctionDecl *FS = Result.Nodes.getNodeAs<clang::FunctionDecl>("entrypoint")) {
      callgraph->VisitFunctionDecl(const_cast<clang::FunctionDecl*>(FS));
    }
  }
};

class ExtendExecInfoFull : public MatchCallback {
  const vector<unsigned char> &bitstream_trace;
  map<llvm::StringRef, unique_ptr<clang::CFG>> &CFGs;
  clang::CallGraph *cg;
public:
  ExtendExecInfoFull(const vector<unsigned char> &bt,
  map<llvm::StringRef, unique_ptr<clang::CFG>> &_CFGs, clang::CallGraph *_cg) : 
    MatchCallback(), bitstream_trace(bt), CFGs(_CFGs), cg(_cg) {}

  ~ExtendExecInfoFull() {}

  virtual void run(const MatchResult &Result) override {
    if (const clang::FunctionDecl *FS = Result.Nodes.getNodeAs<clang::FunctionDecl>("entrypoint")) {
      llvm::errs() << "Add trace info " << FS->getName() << "\n";
      clang::StatInfASTExtendExecInfoDecl extend(FS->getASTContext(), FS->getName(), bitstream_trace, IsStructuralAnalysis, IsTemporalAnalysis, TimeBitsSize);
      while(!extend.EOBS()) {
        extend.VisitFunctionDecl(const_cast<clang::FunctionDecl*>(FS));
      }
      for(auto mb : extend.getMissingFunctionBody())
        llvm::errs() << "Missing function body: " << mb << "\n";
    }
  }
};

class ExtendExecInfoSplitExec : public MatchCallback {
  const vector<unsigned char> &bitstream_trace;
  map<llvm::StringRef, unique_ptr<clang::CFG>> &CFGs;
  clang::CallGraph *cg;
public:
  ExtendExecInfoSplitExec(const vector<unsigned char> &bt,
  map<llvm::StringRef, unique_ptr<clang::CFG>> &_CFGs, clang::CallGraph *_cg) : 
    MatchCallback(), bitstream_trace(bt), CFGs(_CFGs), cg(_cg) {}

  ~ExtendExecInfoSplitExec() {}

  virtual void run(const MatchResult &Result) override {
    if (const clang::FunctionDecl *FS = Result.Nodes.getNodeAs<clang::FunctionDecl>("entrypoint")) {
      llvm::errs() << "Add trace info " << FS->getName() << "\n";
      clang::StatInfASTExtendExecInfoDecl extend(FS->getASTContext(), FS->getName(), bitstream_trace, IsStructuralAnalysis, IsTemporalAnalysis, TimeBitsSize);
      clang::StatInfASTResetExecInfoDecl reset(FS->getASTContext());
      size_t exec_count = 0;
      while(!extend.EOBS()) {
        ++exec_count;
        extend.VisitFunctionDecl(const_cast<clang::FunctionDecl*>(FS));
        exportCFGs(SplitDir+"/exec_"+to_string(exec_count), FS->getASTContext(), CFGs, cg);
        reset.VisitFunctionDecl(const_cast<clang::FunctionDecl*>(FS));
      }
      for(auto mb : extend.getMissingFunctionBody())
        llvm::errs() << "Missing function body: " << mb << "\n";
    }
  }
};

static unique_ptr<clang::ASTUnit> extractCFG(unique_ptr<clang::ASTUnit> ast, ListCFGs &CFGs) {
  // Configure the CFG builder
  clang::CFG::BuildOptions Options;
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

static unique_ptr<clang::ASTUnit> extractCallGraph(unique_ptr<clang::ASTUnit> ast, clang::CallGraph *cg) {
  am::DeclarationMatcher entrypoint_match = am::functionDecl(am::hasName(EntryPoint)).bind("entrypoint");
  cg->shouldVisitRecursively(true);
  CallGraphExtract extractor(cg);
  am::MatchFinder Finder;
  Finder.addMatcher(entrypoint_match, &extractor);
  Finder.matchAST(ast->getASTContext());
  return move(ast);
}

} // namespace

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  EntryPoint.setInitialValue("main");
  TimeBitsSize.setInitialValue(32);

  auto ExpectedParser =
      CommonOptionsParser::create(argc, argv, CFGExtendExecInfoCat, llvm::cl::NumOccurrencesFlag::ZeroOrMore);
  if (!ExpectedParser) {
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }

  if(!IsStructuralAnalysis && !IsTemporalAnalysis) {
    llvm::errs() << "Please specify at least one of --structural or --temporal parameters\n";
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }

  if(!OutputDot && !OutputJson) {
    llvm::errs() << "Please specify at least one of --dot or --json parameters\n";
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }

  CommonOptionsParser &OptionsParser = ExpectedParser.get();

  // Get the content of the bitstream trace file
  vector<uint8_t> *bitstream_trace = nullptr;
  if(!BitstreamFile.empty()) {
    ifstream bitstream_fs(BitstreamFile, ios::binary);
    if(!bitstream_fs.is_open()) {
      llvm::errs() << "Error opening " << BitstreamFile << "\n";
      return 1;
    }
    bitstream_trace = new vector<uint8_t>(istreambuf_iterator<char>(bitstream_fs), {});
    bitstream_fs.close();
  }

  vector<string> args{"-Wno-int-conversion", 
    "-Wno-unused-value", 
    "-Wno-implicit-function-declaration", 
    "-Wno-shift-count-overflow",
    "-Wno-parentheses-equality",
    "-Wno-main-return-type",
    "-Wno-missing-declarations"};

  shared_ptr<clang::PCHContainerOperations> PCHContainerOps = make_shared<clang::PCHContainerOperations>();
  vector<string> AbsolutePaths;
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
    AbsolutePaths.push_back(move(*AbsPath));
  }

  if(!InputDir.empty() && !AbsolutePaths.empty()) {
    errs() << "Can't provide a list of files and an input-dir to scan for source files.\n";
    return 1;
  }

  // Scan for additional C files and directories to put in the include paths from a given root project
  scandir(*OverlayFileSystem, InputDir, IncludePath, AbsolutePaths);

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
  unique_ptr<clang::ASTUnit> ast = buildASTFromCode("", "empty.c");

  //Build all other ASTs and merge them into the empty one
  for (llvm::StringRef File : AbsolutePaths) {
    //get file content
    ifstream ifs(File.str());
    string content( (istreambuf_iterator<char>(ifs) ),
                      (istreambuf_iterator<char>()    ) );
    clang::tooling::FileContentMappings files;
    files.push_back(make_pair(File.str(), content));

    //build ast
    unique_ptr<clang::ASTUnit> tmp_ast = buildASTFromCodeWithArgs(
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
        consumeError(move(Err));
        return 1;
      }
    }
  }

  ListCFGs CFGs;
  ast = extractCFG(move(ast), CFGs);
  clang::CallGraph cg;
  ast = extractCallGraph(move(ast), &cg);

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
