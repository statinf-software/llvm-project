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

static cl::OptionCategory StatInfInstrCategory("statinf-instrumentation options");
static const opt::OptTable &Options = cd::getDriverOptTable();
static cl::opt<string>
    EntryPoint(
        "entrypoint",
        cl::desc("Entrypoint function name to start the CallGraph"),
        cl::cat(StatInfInstrCategory)
    );
static cl::list<string>
    IncludePath("I", cl::desc(Options.getOptionHelpText(cd::options::OPT_include)),
                  cl::cat(StatInfInstrCategory));
static cl::list<string>
    Definitions("D", cl::desc(Options.getOptionHelpText(cd::options::OPT_defsym)),
                  cl::cat(StatInfInstrCategory));
static cl::opt<bool>
    OptDisStructAnalysis("no-structural", 
                  cl::desc("Disable structural analysis instrumentation."),
                  cl::cat(StatInfInstrCategory));
static cl::opt<bool>
    OptDisTempAnalysis("no-temporal", 
                  cl::desc("Disable temporal analysis instrumentation"),
                  cl::cat(StatInfInstrCategory));
static cl::opt<string>
    InstrMacroDefFilePath(
      "instr-macro-def",
      cl::desc("Fullname of the file with the macro definition, default current_source_dir/statinf_instrumentation.h"),
      cl::cat(StatInfInstrCategory)
    );
static cl::opt<string>
    Out(
      "o",
      cl::desc("File in which storing the output"),
      cl::cat(StatInfInstrCategory)
    );
static cl::list<string>
    InputDir("input-dir", 
      cl::desc("Recursively scans this directory to find all .c files, also add all found directories in the include path"),
      cl::cat(StatInfInstrCategory)
    );
static cl::opt<string>
    Intermediate("intermediate-file",
      cl::desc("Export intermediate C file"),
      cl::cat(StatInfInstrCategory)
    );
                
namespace {

class CallGraphExtract : public MatchCallback {
  CallGraph *callgraph;
public:
  explicit CallGraphExtract(CallGraph *cg) : callgraph(cg) {}

  virtual void run(const MatchResult &Result) {
    if (const FunctionDecl *FS = Result.Nodes.getNodeAs<FunctionDecl>("entrypoint")) {
      callgraph->VisitFunctionDecl(const_cast<FunctionDecl*>(FS));
    }
  }
};

class StatInfPrinterLauncher : public ASTConsumer {
  raw_ostream &OS;
  CallGraph *callgraph;
  bool disable_structural;
  bool disable_temporal;

public:
  explicit StatInfPrinterLauncher(raw_ostream &os, CallGraph *cg, bool dis_st, bool dis_temp) : 
    ASTConsumer(), OS(os), callgraph(cg),
    disable_structural(dis_st), disable_temporal(dis_temp) {}

  void HandleTranslationUnit(ASTContext &Context) override {
    StatInfInstrDeclPrinter printer(OS, Context.getPrintingPolicy(), 
      Context, callgraph, 
      disable_structural, disable_temporal, 
      InstrMacroDefFilePath, EntryPoint);
    auto TU = Context.getTranslationUnitDecl();
    printer.Visit(TU);
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
        ct::CommonOptionsParser::create(argc, argv, StatInfInstrCategory, cl::NumOccurrencesFlag::ZeroOrMore);
    if (!ExpectedParser) {
        errs() << ExpectedParser.takeError();
        return 1;
    }
    ct::CommonOptionsParser &OptionsParser = ExpectedParser.get();

    if(EntryPoint.empty()) {
      EntryPoint = "main";
    }
    if(InstrMacroDefFilePath.empty()) {
      InstrMacroDefFilePath = "statinf_instrumentation.h";
    }

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

    unique_ptr<raw_ostream> OutFile = nullptr;
    if(!Out.empty()) {
      error_code EC;
      OutFile = make_unique<raw_fd_ostream>(Out, EC);
      if (EC) {
        errs() << EC.message() << "\n";
        return -1;
      }
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
    ast_matchers::DeclarationMatcher entrypoint_match = am::functionDecl(am::hasName(EntryPoint)).bind("entrypoint");
    CallGraph cg;
    cg.shouldVisitRecursively(true);
    CallGraphExtract extractor(&cg);
    am::MatchFinder Finder;
    Finder.addMatcher(entrypoint_match, &extractor);
    Finder.matchAST(ast->getASTContext());

    // Parse the AST to print it and add the call to the instrumentation macros
    string new_code;
    raw_string_ostream new_code_stream(new_code);
    StatInfPrinterLauncher launch(new_code_stream, &cg, !OptDisStructAnalysis.getValue(), !OptDisTempAnalysis.getValue());
    launch.Initialize(ast->getASTContext());
    launch.HandleTranslationUnit(ast->getASTContext());
    if(!Intermediate.empty()) {
      error_code EC;
      raw_fd_ostream ios(Intermediate, EC);
      if (EC) {
        errs() << EC.message() << "\n";
        return -1;
      }
      ios << new_code << "\n";
    }

    // Finally re-preprocess to unroll StatInf instrumentation macros
    unique_ptr<ASTUnit> final_ast = ct::buildASTFromCodeWithArgs(
      new_code, args, "/tmp/file.c",
      "statinf-instrumentation", PCHContainerOps
    );
    // unique_ptr<ASTConsumer> final_printer = CreateASTPrinter(move(OutFile), "");
    // final_printer->Initialize(final_ast->getASTContext());
    // final_printer->HandleTranslationUnit(final_ast->getASTContext());
    StatInfPrinterLauncher launch2(OutFile ? *(OutFile.get()) : outs(), &cg, false, false);
    launch2.Initialize(final_ast->getASTContext());
    launch2.HandleTranslationUnit(final_ast->getASTContext());

    return 0;
}