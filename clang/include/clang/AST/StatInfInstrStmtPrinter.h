//===- StatInfInstrStmtPrinter.cpp - Printing implementation for Stmt ASTs ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the Stmt::dumpPretty/Stmt::printPretty methods, which
// pretty print the AST back out to C code.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclOpenMP.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/ExprOpenMP.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/OpenMPClause.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/StmtObjC.h"
#include "clang/AST/StmtOpenMP.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TemplateBase.h"
#include "clang/AST/Type.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/ExpressionTraits.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/JsonSupport.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/Lambda.h"
#include "clang/Basic/OpenMPKinds.h"
#include "clang/Basic/OperatorKinds.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TypeTraits.h"
#include "clang/Lex/Lexer.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <optional>
#include <string>

//===----------------------------------------------------------------------===//
// StatInfInstrStmtPrinter Visitor
//===----------------------------------------------------------------------===//

namespace clang {

  class StatInfInstrDeclPrinter;
  class StatInfInstrStmtPrinter : public StmtVisitor<StatInfInstrStmtPrinter> {
    raw_ostream &OS;
    StatInfInstrDeclPrinter *declprinter;
    unsigned IndentLevel;
    PrinterHelper* Helper;
    PrintingPolicy Policy;
    std::string NL;
    const ASTContext *Context;
    bool enable_instrumentation = false; // enable instrumentation in general, used only for functions reachable from an given entrypoint
    bool enter_function_body = false; // next CompoundStmt is the body of a function
    bool enter_loop_body = false; // next CompoundStmt is the body of a loop
    bool enter_then_body = false; // next CompoundStmt is the body of a then
    bool enter_else_body = false; // next CompoundStmt is the body of a else
    bool enter_main_function = false; // next CompoundStmt is the body of the main function
    uint32_t switch_case_count = 0; // counts how much case a SwitchStmt has
    bool EnableStructuralAnalysis = true; // Enable the addition of the macros only for the structural analysis
    bool EnableTemporalAnalysis = true; // Enable the addition of the macros only for the temporal analysis
    bool entry_point_func = false; // next CompoundStmt is the configured entrypoint
    bool ret_entry_point_func = false; // keep the state of the entrypoint for return instruction

  public:
    StatInfInstrStmtPrinter(raw_ostream &os, StatInfInstrDeclPrinter *dp, PrinterHelper *helper,
                const PrintingPolicy &Policy, unsigned Indentation = 0,
                StringRef NL = "\n", const ASTContext *Context = nullptr,
                bool esa=true, bool eta=true)
        : OS(os), declprinter(dp), IndentLevel(Indentation), Helper(helper), Policy(Policy),
          NL(NL), Context(Context), EnableStructuralAnalysis(esa), EnableTemporalAnalysis(eta) {}

    void PrintStmt(Stmt *S) { PrintStmt(S, Policy.Indentation); }

    void PrintStmt(Stmt *S, int SubIndent) {
      IndentLevel += SubIndent;
      if (S && isa<Expr>(S)) {
        // If this is an expr used in a stmt context, indent and newline it.
        Indent();
        Visit(S);
        OS << ";" << NL;
      } else if (S) {
        Visit(S);
      } else {
        Indent() << "<<<NULL STATEMENT>>>" << NL;
      }
      IndentLevel -= SubIndent;
    }

    void PrintInitStmt(Stmt *S, unsigned PrefixWidth) {
      // FIXME: Cope better with odd prefix widths.
      IndentLevel += (PrefixWidth + 1) / 2;
      if (auto *DS = dyn_cast<DeclStmt>(S))
        PrintRawDeclStmt(DS);
      else
        PrintExpr(cast<Expr>(S));
      OS << "; ";
      IndentLevel -= (PrefixWidth + 1) / 2;
    }

    void PrintControlledStmt(Stmt *S) {
      if (auto *CS = dyn_cast<CompoundStmt>(S)) {
        OS << " ";
        PrintRawCompoundStmt(CS);
        OS << NL;
      } else {
        OS << NL;
        PrintStmt(S);
      }
    }

    void PrintRawCompoundStmt(CompoundStmt *S);
    void PrintRawDecl(Decl *D);
    void PrintRawDeclStmt(const DeclStmt *S);
    void PrintRawIfStmt(IfStmt *If);
    void PrintRawCXXCatchStmt(CXXCatchStmt *Catch);
    void PrintCallArgs(CallExpr *E);
    void PrintRawSEHExceptHandler(SEHExceptStmt *S);
    void PrintRawSEHFinallyStmt(SEHFinallyStmt *S);
    void PrintOMPExecutableDirective(OMPExecutableDirective *S,
                                     bool ForceNoStmt = false);
    void PrintFPPragmas(CompoundStmt *S);

    void PrintExpr(Expr *E) {
      if (E)
        Visit(E);
      else
        OS << "<null expr>";
    }

    raw_ostream &Indent(int Delta = 0) {
      for (int i = 0, e = IndentLevel+Delta; i < e; ++i)
        OS << "  ";
      return OS;
    }

    void Visit(Stmt* S) {
      if (Helper && Helper->handledStmt(S,OS))
          return;
      else StmtVisitor<StatInfInstrStmtPrinter>::Visit(S);
    }

    void VisitStmt(Stmt *Node) LLVM_ATTRIBUTE_UNUSED {
      Indent() << "<<unknown stmt type>>" << NL;
    }

    void VisitExpr(Expr *Node) LLVM_ATTRIBUTE_UNUSED {
      OS << "<<unknown expr type>>";
    }

    void VisitCXXNamedCastExpr(CXXNamedCastExpr *Node);

    void SetEnterFunctionBody() { enter_function_body = true;}
    void SetEnterLoopBody() { enter_loop_body = true;}
    void SetEnterThenBody() { enter_then_body = true;}
    void SetEnterElseBody() { enter_else_body = true;}
    void SetEnterMainFunction() { enter_main_function = true;}
    void SetEnableInstrumentation() { enable_instrumentation = true;}
    void SetDisableInstrumentation() { enable_instrumentation = false;}
    void SetEnterEntryPointFunc() { entry_point_func = true;}

#define ABSTRACT_STMT(CLASS)
#define STMT(CLASS, PARENT) \
    void Visit##CLASS(CLASS *Node);
#include "clang/AST/StmtNodes.inc"
  };

} // namespace