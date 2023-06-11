#include "clang/AST/ASTConsumer.h"
#include "clang/Frontend/ASTConsumers.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Signals.h"
#include "clang/AST/StatInfInstrDeclPrinter.h"
#include "clang/Frontend/CompilerInstance.h"

#include "../StatInfCommon/StatInfUtils.h"

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
static cl::opt<string>
    Filter("filter",
      cl::desc("Filter out files that matches the given pattern, the full path is checked so a full directory can been filter out."),
      cl::cat(StatInfInstrCategory)
    );
                
namespace {

class StatInfPrinterLauncher : public ASTConsumer {
  raw_ostream &OS;
  CallGraph *callgraph;
  bool enable_structural;
  bool enable_temporal;

public:
  explicit StatInfPrinterLauncher(raw_ostream &os, CallGraph *cg, bool en_st, bool en_temp) : 
    ASTConsumer(), OS(os), callgraph(cg),
    enable_structural(en_st), enable_temporal(en_temp) {}

  void HandleTranslationUnit(ASTContext &Context) override {
    StatInfInstrDeclPrinter printer(OS, Context.getPrintingPolicy(), 
      Context, callgraph, 
      enable_structural, enable_temporal, 
      InstrMacroDefFilePath, EntryPoint);
    auto TU = Context.getTranslationUnitDecl();
    printer.Visit(TU);
  }
};

static error_code processASTUnit(const string &input_file, const string &output_file, const string &intermediate_file, 
  unique_ptr<ASTUnit> current_ast, CallGraph *cg, const vector<string> &args,
  DiagnosticConsumer *diagcons) {

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

  shared_ptr<PCHContainerOperations> PCHContainerOps = make_shared<PCHContainerOperations>();
  // Finally re-preprocess to unroll StatInf instrumentation macros
  unique_ptr<ASTUnit> final_ast = ct::buildASTFromCodeWithArgs(
    new_code, args, intermediate_file, "statinf-instrumentation",PCHContainerOps,
      tooling::getClangStripDependencyFileAdjuster(), tooling::FileContentMappings(),
      diagcons
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
      errs() << "Please provide an output dir when providing an input dir\n";
      return 1;
    }

    vector<string> C_files, other_files;
    map<string,string> pp_c_match;
    IntrusiveRefCntPtr<vfs::OverlayFileSystem> OverlayFileSystem(
      new vfs::OverlayFileSystem(vfs::getRealFileSystem())
    );

    get_c_files_from_cmdline(C_files, *OverlayFileSystem, OptionsParser);

    if(!InputDir.empty() && !C_files.empty()) {
      errs() << "Can't provide a list of files and an input-dir to scan for source files.\n";
      return 1;
    }

    if(InputDir.empty() && !C_files.empty()) {
      errs() << "To provide multiple source files, we only allow to have them within the same input dir, provided with the --input-dir option\n";
      return 1;
    }

    string full_input_dir_path = get_full_path(InputDir);
    string full_output_dir_path = get_create_full_path(OutputDir, *OverlayFileSystem);
    if(!full_input_dir_path.empty() && full_input_dir_path == full_output_dir_path) {
      errs() << "Output dir and Input dir must be different\n";
      return 1;
    }

    if(!full_input_dir_path.empty()) {
      // Scan for additional C files and directories to put in the include paths from a given root project
      scandir(*OverlayFileSystem, full_input_dir_path, IncludePath, C_files, other_files, pp_c_match, Filter);
    }

    if(C_files.size() > 1 && !Out.empty()) {
      errs() << "Can't provide the -o option if there is more that one source file to handle, use --output-dir instead\n";
      return 1;
    }

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

    clang::CallGraph cg;
    extractCallGraph(ast.get(), &cg, EntryPoint);

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
      
      auto printer = CreateASTPrinter(nullptr, "");
      printer->Initialize(astunit.second->getASTContext());
      printer->HandleTranslationUnit(astunit.second->getASTContext());

      error_code ec = processASTUnit(astunit.first.str(), Out, intermediate_file, move(astunit.second), &cg, args_for_clangtool, &diagprinter);
      if(ec.value()) {
        errs() << ec.message() << "\n";
        return EXIT_FAILURE;
      }
    }
    else {
      for(auto &astunit : ast_books) {
        unique_ptr<ASTUnit> &current_ast = astunit.second;

        string output_file = astunit.first.str();
        bool pp_used = false;
        if(pp_c_match.count(output_file)) {
          output_file = pp_c_match[output_file];
          pp_used = true;
        }
        output_file = replaceAll(output_file, full_input_dir_path, full_output_dir_path);

        string intermediate_file = replaceAll(output_file, ".c", ".inter.c");

        error_code ec = processASTUnit(astunit.first.str(), output_file, intermediate_file, move(current_ast), &cg, args_for_clangtool, &diagprinter);
        if(ec.value()) {
          errs() << ec.message() << "\n";
          return EXIT_FAILURE;
        }
      }
      for(string file : other_files) {
        string output_file = replaceAll(file, full_input_dir_path, full_output_dir_path);
        ifstream ifs(file);
        string content( (istreambuf_iterator<char>(ifs) ),
                          (istreambuf_iterator<char>()    ) );
        string output_dir = output_file.substr(0, output_file.find_last_of("/"));
        error_code ec = create_directory_recursive(output_dir);
        if(ec.value()) {
          errs() << output_dir << ": ";
          return 1;
        }
        ofstream ofs(output_file);
        ofs << content;
      }
    }

    return 0;
}