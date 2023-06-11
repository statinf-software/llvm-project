#include "StatInfUtils.h"

using namespace llvm;
using namespace clang;
using namespace std;
namespace cd = clang::driver;
namespace ct = clang::tooling;
namespace am = clang::ast_matchers;

vector<string> args_for_clangtool{"-Wno-int-conversion", 
    "-Wno-unused-value", 
    "-Wno-implicit-function-declaration", 
    "-Wno-shift-count-overflow",
    "-Wno-parentheses-equality",
    "-Wno-main-return-type",
    "-Wno-missing-declarations",
    "-Wno-int-to-pointer-cast",
    "-Wno-pointer-to-int-cast",
    "-Wno-incompatible-library-redeclaration",
    "-Wno-switch",
    "-Wno-literal-conversion",
    "-Wno-unused-command-line-argument",
    "-Wno-error"
    };

string get_full_path(string dir) {
    if(!dir.empty()) {
      SmallVector<char> tmp_path;
      sys::fs::real_path(dir, tmp_path, true);
      return string(tmp_path.data(), tmp_path.size());
    }
    return "";
}

string get_create_full_path(string dir, vfs::FileSystem &FS) {
    if(!dir.empty()) {
        string tmp_full_output_dir_path = *(ct::getAbsolutePath(FS, dir));
        error_code ec = sys::fs::create_directory(tmp_full_output_dir_path);
        if(ec.value()) {
            errs() << ec.message() << " -- " << tmp_full_output_dir_path << "\n";
            return "";
        }
        SmallVector<char> tmp_path;
        sys::fs::real_path(tmp_full_output_dir_path, tmp_path, true);
        return string(tmp_path.data(), tmp_path.size());
    }
    return "";
}

void add_include_paths_in_clangtoolarg(cl::list<string> &paths, llvm::vfs::FileSystem &FS) {
    for(auto I : paths) {
      auto AbsPath = ct::getAbsolutePath(FS, I);
      if (!AbsPath) {
        errs() << "Skipping " << I
                    << ". Error while getting an absolute path: "
                    << toString(AbsPath.takeError()) << "\n";
        continue;
      }
      args_for_clangtool.push_back("-I"+string(AbsPath->c_str()));
    }
}

void add_defs_in_clangtoolarg(cl::list<string> &defs) {
    for(auto D : defs) {
      args_for_clangtool.push_back("-D"+D);
    }
}

int build_ast_book(map<StringRef, unique_ptr<ASTUnit>> &ast_book, 
  ASTUnit *app_ast, 
  const vector<string> &files, const vector<string> &args,
  DiagnosticConsumer *diagcons) {
  shared_ptr<PCHContainerOperations> PCHContainerOps = make_shared<PCHContainerOperations>();
  for (StringRef File : files) {
    //get file content
    ifstream ifs(File.str());
    string content( (istreambuf_iterator<char>(ifs) ),
                      (istreambuf_iterator<char>()    ) );
    tooling::FileContentMappings files;
    files.push_back(make_pair(File.str(), content));

    //build ast
    unique_ptr<ASTUnit> tmp_ast = ct::buildASTFromCodeWithArgs(
        content, args, File.str(), "statinf-CFG-with-trace", PCHContainerOps,
        tooling::getClangStripDependencyFileAdjuster(), files,
        diagcons
    );
    if(tmp_ast == nullptr) {
      errs() << "No AST have been built for " << File.str() << "\n";
      return -1;
    }

    //import each toplevel declaration one by one
    ASTImporter Importer(app_ast->getASTContext(), app_ast->getFileManager(),
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
    ast_book[File] = move(tmp_ast);
  }
  return 0;
}

void scandir(vfs::FileSystem &fs, StringRef dirname, cl::list<string> &dirs, vector<string> &C_files, vector<string> &other_files, map<string,string> &pp_c_match, string Filter) {
  dirs.push_back(*(ct::getAbsolutePath(fs, dirname)));
  error_code EC;
  vector<string> pp_cfiles;
  for(vfs::directory_iterator elt = fs.dir_begin(dirname, EC), dirend ; elt != dirend && !EC; elt.increment(EC)) {
    if(!Filter.empty() && elt->path().str().find(Filter) != string::npos)
      continue;
    if(elt->type() == sys::fs::file_type::directory_file)
      scandir(fs, elt->path(), dirs, C_files, other_files, pp_c_match, Filter);
    else if (elt->path().endswith(".c") || elt->path().endswith(".cla"))
      C_files.push_back(*(ct::getAbsolutePath(fs, elt->path())));
    else if (elt->path().endswith(".pp")) 
      pp_cfiles.push_back(*(ct::getAbsolutePath(fs, elt->path())));
    else 
      other_files.push_back(*(ct::getAbsolutePath(fs, elt->path())));
  }
  
  if(!pp_cfiles.empty()) {
    for(string ppfile : pp_cfiles) {
      size_t pos = ppfile.find_last_of('.');
      string ppfile_noext = ppfile.substr(0, pos);
      auto it = find_if(C_files.begin(), C_files.end(), [ppfile_noext](const string &cfile) {
        size_t pos = cfile.find_last_of('.');
        return ppfile_noext == cfile.substr(0, pos);
      });
      if(it != C_files.end()) {
        pp_c_match[ppfile] = *it;
        C_files.erase(it);
        C_files.push_back(ppfile);
      }
    }

    //If we have some PP files, then we only want PP files not a mix of .c/.cla
    vector<vector<string>::iterator> to_delete;
    for(auto it = C_files.begin(), et=C_files.end() ; it != et ; ++it) {
      size_t pos = it->find_last_of('.');
      string ext = it->substr(pos, it->size());
      if(ext == ".c" || ext == ".cla") {
        to_delete.push_back(it);
      }
    }
    for(auto it : to_delete) {
      C_files.erase(it);
    }
  } 
}

error_code create_directory_recursive(StringRef dir) {
  if(dir.empty())
    return error_code();
  StringRef parent_dir = dir.substr(0, dir.find_last_of("/"));
  error_code ec = create_directory_recursive(parent_dir);
  if(ec.value() != 0)
    return ec;

  return sys::fs::create_directory(dir);
}

void extractCallGraph(ASTUnit *ast, CallGraph *cg, string EntryPoint) {
  am::DeclarationMatcher entrypoint_match = am::functionDecl(am::hasName(EntryPoint)).bind("entrypoint");
  cg->shouldVisitRecursively(true);
  CallGraphExtract extractor(cg);
  am::MatchFinder Finder;
  Finder.addMatcher(entrypoint_match, &extractor);
  Finder.matchAST(ast->getASTContext());
}

void get_c_files_from_cmdline(vector<string> &C_files, llvm::vfs::FileSystem &FS, ct::CommonOptionsParser &OptionsParser) {
  // Compute all absolute paths before we run any actions, as those will change
  // the working directory.
  C_files.reserve(OptionsParser.getSourcePathList().size());
  for (const auto &SourcePath : OptionsParser.getSourcePathList()) {
    auto AbsPath = ct::getAbsolutePath(FS, SourcePath);
    if (!AbsPath) {
      errs() << "Skipping " << SourcePath
                  << ". Error while getting an absolute path: "
                  << toString(AbsPath.takeError()) << "\n";
      continue;
    }
    C_files.push_back(move(*AbsPath));
  }
}