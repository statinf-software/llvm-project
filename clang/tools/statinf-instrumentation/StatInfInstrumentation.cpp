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
      cl::desc("File in which storing the output. Only available if a single file is given"),
      cl::cat(StatInfInstrCategory)
    );
static cl::opt<string>
    InputDir("input-dir", 
      cl::desc("Recursively scans this directory to find all .c files, also add all found directories in the include path"),
      cl::cat(StatInfInstrCategory)
    );
static cl::opt<string>
    OutputDir("output-dir", 
      cl::desc("If multiple files are given their instrumented version will be stored there"),
      cl::cat(StatInfInstrCategory)
    );
static cl::opt<bool>
    Intermediate("debug-intermediate",
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

static error_code create_directory_recursive(StringRef dir) {
  if(dir.empty())
    return error_code();
  StringRef parent_dir = dir.substr(0, dir.find_last_of("/"));
  error_code ec = create_directory_recursive(parent_dir);
  if(ec.value() != 0)
    return ec;

  return sys::fs::create_directory(dir);
}

static error_code processASTUnit(const string &input_file, const string &output_file, const string &intermediate_file, 
  unique_ptr<ASTUnit> current_ast, CallGraph *cg, const vector<string> &args) {

  string output_dir = output_file.substr(0, output_file.find_last_of("/"));
  error_code ec = create_directory_recursive(output_dir);
  if(ec.value()) {
    errs() << output_dir << ": ";
    return ec;
  }

  // Parse the AST to print it and add the call to the instrumentation macros
  string new_code;
  raw_string_ostream new_code_stream(new_code);
  StatInfPrinterLauncher launch(new_code_stream, cg, !OptDisStructAnalysis.getValue(), !OptDisTempAnalysis.getValue());
  launch.Initialize(current_ast->getASTContext());
  launch.HandleTranslationUnit(current_ast->getASTContext());
  if(Intermediate) {
    ec.clear();
    raw_fd_ostream ios(intermediate_file, ec);
    if (ec) {
      errs() << intermediate_file << ": ";
      return ec;
    }
    ios << new_code << "\n";
  }

  // Finally re-preprocess to unroll StatInf instrumentation macros
  unique_ptr<ASTUnit> final_ast = ct::buildASTFromCodeWithArgs(
    new_code, args, intermediate_file, "statinf-instrumentation"
  );
  ec.clear();
  raw_fd_ostream OutFile(output_file, ec);
  if (ec) {
    errs() << output_file << ": ";
    return ec;
  }
  StatInfPrinterLauncher launch2(OutFile, cg, false, false);
  launch2.Initialize(final_ast->getASTContext());
  launch2.HandleTranslationUnit(final_ast->getASTContext());
  return error_code();
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

    if(!InputDir.empty() && OutputDir.empty()) {
      OutputDir = (string)InputDir;
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

    if(!InputDir.empty() && !AbsolutePaths.empty()) {
      errs() << "Can't provide a list of files and an input-dir to scan for source files.\n";
      return 1;
    }

    // Scan for additional C files and directories to put in the include paths from a given root project
    scandir(*OverlayFileSystem, InputDir, IncludePath, AbsolutePaths);

    if(AbsolutePaths.size() > 1 && !Out.empty()) {
      errs() << "Can't provide the -o option if there is more that one source file to handle, use --output-dir instead\n";
      return 1;
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

    map<StringRef, unique_ptr<ASTUnit>> ast_books;

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
      ast_books[File] = move(tmp_ast);
    }

    // Extract the call graph from the given entrypoint
    ast_matchers::DeclarationMatcher entrypoint_match = am::functionDecl(am::hasName(EntryPoint)).bind("entrypoint");
    CallGraph cg;
    cg.shouldVisitRecursively(true);
    CallGraphExtract extractor(&cg);
    am::MatchFinder Finder;
    Finder.addMatcher(entrypoint_match, &extractor);
    Finder.matchAST(ast->getASTContext());

    if(ast_books.size() == 1) {
      auto &astunit = *(ast_books.begin());

      string intermediate_file;
      if(Out.empty()) {
        Out = astunit.first.str();
        Out = replaceAll(Out, ".c", ".inst.c"); //FIXME: find a way to get the extension depending on language setting 
        intermediate_file = replaceAll(Out, ".inst.c", ".inter.c");
      }
      else
        intermediate_file = replaceAll(Out, ".c", ".inter.c");
      
      error_code ec = processASTUnit(astunit.first.str(), Out, intermediate_file, move(astunit.second), &cg, args);
      if(ec.value()) {
        errs() << ec.message() << "\n";
        return EXIT_FAILURE;
      }
    }
    else {
      // Get input folder
      string full_input_dir_path;
      if(!InputDir.empty()) {
        SmallVector<char> tmp_path;
        sys::fs::real_path(InputDir, tmp_path, true);
        full_input_dir_path = string(tmp_path.data(), tmp_path.size());
      }
      // Prepare output folder
      string full_output_dir_path;
      if(!OutputDir.empty()) {
        string tmp_full_output_dir_path = *(ct::getAbsolutePath(*OverlayFileSystem, OutputDir));
        error_code ec = sys::fs::create_directory(tmp_full_output_dir_path);
        if(ec.value()) {
          errs() << ec.message() << " -- " << tmp_full_output_dir_path << "\n";
          return 1;
        }
        SmallVector<char> tmp_path;
        sys::fs::real_path(tmp_full_output_dir_path, tmp_path, true);
        full_output_dir_path = string(tmp_path.data(), tmp_path.size());
      }

      for(auto &astunit : ast_books) {
        unique_ptr<ASTUnit> &current_ast = astunit.second;

        // Get input folder
        if(InputDir.empty()) {
          string current_file = astunit.first.str();
          SmallVector<char> tmp_path;
          sys::fs::real_path(current_file.substr(0, current_file.find_last_of("/")), tmp_path, true);
          full_input_dir_path = string(tmp_path.data(), tmp_path.size());
        }


        string output_file = astunit.first.str();
        if(!full_output_dir_path.empty())
          output_file = replaceAll(output_file, full_input_dir_path, full_output_dir_path);
        output_file = replaceAll(output_file, ".c", ".inst.c"); //FIXME: find a way to get the extension depending on language setting

        string intermediate_file = replaceAll(output_file, ".inst.c", ".inter.c");

        error_code ec = processASTUnit(astunit.first.str(), output_file, intermediate_file, move(current_ast), &cg, args);
        if(ec.value()) {
          errs() << ec.message() << "\n";
          return EXIT_FAILURE;
        }
      }
    }

    return 0;
}