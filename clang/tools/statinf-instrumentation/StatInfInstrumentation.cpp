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

using namespace clang::driver;
using namespace clang::tooling;
using namespace llvm;
using namespace std;
using MatchResult = clang::ast_matchers::MatchFinder::MatchResult;
using namespace clang::ast_matchers;

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp(
    "To use this tool ...:\n"
    "Options:\n"
);

static cl::OptionCategory StatInfInstrCategory("statinf-instrumentation options");
static const opt::OptTable &Options = getDriverOptTable();
static cl::opt<std::string>
    EntryPoint(
        "entrypoint",
        cl::desc("Entrypoint function name to start the CallGraph"),
        cl::cat(StatInfInstrCategory)
    );
static cl::list<std::string>
    IncludePath("I", cl::desc(Options.getOptionHelpText(options::OPT_include)),
                  cl::cat(StatInfInstrCategory));
static cl::list<std::string>
    Definitions("D", cl::desc(Options.getOptionHelpText(options::OPT_defsym)),
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
static cl::opt<std::string>
    Out(
      "o",
      cl::desc("File in which storing the output"),
      cl::cat(StatInfInstrCategory)
    );
static cl::list<std::string>
    InputDir("input-dir", 
      cl::desc("Recursively scans this directory to find all .c files, also add all found directories in the include path"),
      cl::cat(StatInfInstrCategory)
    );

                
namespace {

class CallGraphExtract : public MatchFinder::MatchCallback {
  clang::CallGraph *callgraph;
public:
  explicit CallGraphExtract(clang::CallGraph *cg) : callgraph(cg) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    if (const clang::FunctionDecl *FS = Result.Nodes.getNodeAs<clang::FunctionDecl>("entrypoint")) {
      callgraph->VisitFunctionDecl(const_cast<clang::FunctionDecl*>(FS));
    }
  }
};

class StatInfPrinterLauncher : public clang::ASTConsumer {
  llvm::raw_ostream &OS;
  clang::CallGraph *callgraph;
  bool disable_structural;
  bool disable_temporal;

public:
  explicit StatInfPrinterLauncher(llvm::raw_ostream &os, clang::CallGraph *cg, bool dis_st, bool dis_temp) : 
    clang::ASTConsumer(), OS(os), callgraph(cg),
    disable_structural(dis_st), disable_temporal(dis_temp) {}

  void HandleTranslationUnit(clang::ASTContext &Context) override {
    clang::StatInfInstrDeclPrinter printer(OS, Context.getPrintingPolicy(), 
      Context, callgraph, 
      disable_structural, disable_temporal, 
      InstrMacroDefFilePath, EntryPoint);
    auto TU = Context.getTranslationUnitDecl();
    printer.Visit(TU);
  }
};

static void scandir(llvm::vfs::FileSystem &fs, StringRef dirname, cl::list<string> &dirs, vector<string> &C_files) {
  dirs.push_back(*(getAbsolutePath(fs, dirname)));
  std::error_code EC;
  for(llvm::vfs::directory_iterator elt = fs.dir_begin(dirname, EC), dirend ; elt != dirend && !EC; elt.increment(EC)) {
    if (elt->path().endswith(".c"))
      C_files.push_back(*(getAbsolutePath(fs, elt->path())));
    else if(elt->type() == llvm::sys::fs::file_type::directory_file)
      scandir(fs, elt->path(), dirs, C_files);
  }
}

} // namespace

int main(int argc, const char **argv) {
    llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

    auto ExpectedParser =
        CommonOptionsParser::create(argc, argv, StatInfInstrCategory, llvm::cl::NumOccurrencesFlag::ZeroOrMore);
    if (!ExpectedParser) {
        llvm::errs() << ExpectedParser.takeError();
        return 1;
    }
    CommonOptionsParser &OptionsParser = ExpectedParser.get();

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

    shared_ptr<clang::PCHContainerOperations> PCHContainerOps = make_shared<clang::PCHContainerOperations>();
    std::vector<std::string> AbsolutePaths;
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
      AbsolutePaths.push_back(std::move(*AbsPath));
    }

    // Scan for additional C files and directories to put in the include paths from a given root project
    for(string edir : InputDir) {
      scandir(*OverlayFileSystem, edir, IncludePath, AbsolutePaths);
    }

    std::unique_ptr<llvm::raw_ostream> OutFile = nullptr;
    if(!Out.empty()) {
      std::error_code EC;
      OutFile = std::make_unique<llvm::raw_fd_ostream>(Out, EC);
      if (EC) {
        llvm::errs() << EC.message() << "\n";
        return -1;
      }
    }

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
    std::unique_ptr<clang::ASTUnit> ast = buildASTFromCode("", "empty.c");

    //Build all other ASTs and merge them into the empty one
    for (llvm::StringRef File : AbsolutePaths) {
      //get file content
      std::ifstream ifs(File.str());
      std::string content( (std::istreambuf_iterator<char>(ifs) ),
                        (std::istreambuf_iterator<char>()    ) );
      clang::tooling::FileContentMappings files;
      files.push_back(make_pair(File.str(), content));

      //build ast
      std::unique_ptr<clang::ASTUnit> tmp_ast = buildASTFromCodeWithArgs(
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
          consumeError(std::move(Err));
          return 1;
        }
      }
    }

    // Extract the call graph from the given entrypoint
    clang::ast_matchers::DeclarationMatcher entrypoint_match = functionDecl(hasName(EntryPoint)).bind("entrypoint");
    clang::CallGraph cg;
    cg.shouldVisitRecursively(true);
    CallGraphExtract extractor(&cg);
    MatchFinder Finder;
    Finder.addMatcher(entrypoint_match, &extractor);
    Finder.matchAST(ast->getASTContext());

    // Parse the AST to print it and add the call to the instrumentation macros
    string new_code;
    llvm::raw_string_ostream new_code_stream(new_code);
    StatInfPrinterLauncher launch(new_code_stream, &cg, !OptDisStructAnalysis.getValue(), !OptDisTempAnalysis.getValue());
    launch.Initialize(ast->getASTContext());
    launch.HandleTranslationUnit(ast->getASTContext());
    // llvm::errs() << new_code << "\n";

    // Finally re-preprocess to unroll StatInf instrumentation macros
    std::unique_ptr<clang::ASTUnit> final_ast = buildASTFromCodeWithArgs(
      new_code, args, "/tmp/file.c",
      "statinf-instrumentation", PCHContainerOps
    );
    // unique_ptr<clang::ASTConsumer> final_printer = clang::CreateASTPrinter(move(OutFile), "");
    // final_printer->Initialize(final_ast->getASTContext());
    // final_printer->HandleTranslationUnit(final_ast->getASTContext());
    StatInfPrinterLauncher launch2(OutFile ? *(OutFile.get()) : llvm::outs(), &cg, false, false);
    launch2.Initialize(final_ast->getASTContext());
    launch2.HandleTranslationUnit(final_ast->getASTContext());

    return 0;
}