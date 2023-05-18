#include "StatInfDiagConsumer.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/TextDiagnostic.h"
#include "clang/Lex/Lexer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Basic/AllDiagnostics.h"
#include <algorithm>
using namespace clang;

StatInfDiagnosticPrinter::StatInfDiagnosticPrinter(raw_ostream &os,
                                             IntrusiveRefCntPtr<DiagnosticOptions> diags)
  : OS(os), DiagOpts(diags){
}

StatInfDiagnosticPrinter::~StatInfDiagnosticPrinter() {
}

void StatInfDiagnosticPrinter::BeginSourceFile(const LangOptions &LO,
                                            const Preprocessor *PP) {
  // Build the TextDiagnostic utility.
  TextDiag.reset(new TextDiagnostic(OS, LO, &*DiagOpts));
}

void StatInfDiagnosticPrinter::EndSourceFile() {
  TextDiag.reset();
}

void StatInfDiagnosticPrinter::HandleDiagnostic(DiagnosticsEngine::Level Level,
                                             const Diagnostic &Info) {
                                              
  //This diagnostic arises within the LHS code when they assign the @ of a function
  // as a value to initialise a struct
  if(Info.getID() == diag::err_init_element_not_constant)
    return;
  //This diagnostic arises when instrumenting LHS code and including in the 
  // statinf_instrumentation.h file a file from the original project that
  // was already including in the original code. Hence the 1st PP step
  // already included the symbol
  if(Info.getID() == diag::err_redefinition || Info.getID() == diag::note_previous_definition)
    return;
                                    
  // Default implementation (Warnings/errors count).
  DiagnosticConsumer::HandleDiagnostic(Level, Info);

  // Render the diagnostic message into a temporary buffer eagerly. We'll use
  // this later as we print out the diagnostic to the terminal.
  SmallString<100> OutStr;
  Info.FormatDiagnostic(OutStr);

  llvm::raw_svector_ostream DiagMessageStream(OutStr);

  // Keeps track of the starting position of the location
  // information (e.g., "foo.c:10:4:") that precedes the error
  // message. We use this information to determine how long the
  // file+line+column number prefix is.
  uint64_t StartOfLocationInfo = OS.tell();

  if (!Prefix.empty())
    OS << Prefix << ": ";

  // Use a dedicated, simpler path for diagnostics without a valid location.
  // This is important as if the location is missing, we may be emitting
  // diagnostics in a context that lacks language options, a source manager, or
  // other infrastructure necessary when emitting more rich diagnostics.
  if (!Info.getLocation().isValid()) {
    TextDiagnostic::printDiagnosticLevel(OS, Level, DiagOpts->ShowColors);
    TextDiagnostic::printDiagnosticMessage(
        OS, /*IsSupplemental=*/Level == DiagnosticsEngine::Note,
        DiagMessageStream.str(), OS.tell() - StartOfLocationInfo,
        DiagOpts->MessageLength, DiagOpts->ShowColors);
    OS.flush();
    return;
  }

  // Assert that the rest of our infrastructure is setup properly.
  assert(DiagOpts && "Unexpected diagnostic without options set");
  assert(Info.hasSourceManager() &&
         "Unexpected diagnostic with no source manager");
  assert(TextDiag && "Unexpected diagnostic outside source file processing");

  TextDiag->emitDiagnostic(
      FullSourceLoc(Info.getLocation(), Info.getSourceManager()), Level,
      DiagMessageStream.str(), Info.getRanges(), Info.getFixItHints());

  OS.flush();
}
