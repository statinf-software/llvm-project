//===- StmtPrinter.cpp - Printing implementation for Stmt ASTs ------------===//
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
// StmtPrinter Visitor
//===----------------------------------------------------------------------===//

namespace clang {
  class StatInfASTExtendExecInfoDecl;

  namespace StatInfASTExtendExecInfoStmt_ns {
    enum STATUS {
      PROCEED,
      BREAK_LOOP,
      CONTINUE_LOOP,
      EXIT_FUNCTION
    };
  }

  class StatInfASTExtendExecInfoStmt;
  using StatInfASTExtendExecInfoStmtParent = StmtVisitor<StatInfASTExtendExecInfoStmt, StatInfASTExtendExecInfoStmt_ns::STATUS>;

  class StatInfASTExtendExecInfoStmt : public StatInfASTExtendExecInfoStmtParent {
    bool enter_func_body = false;

    StatInfASTExtendExecInfoDecl *declvisitor;

    // bool enabled_structural;
    // bool enabled_temporal;

  public:
    StatInfASTExtendExecInfoStmt(StatInfASTExtendExecInfoDecl *dv/*, bool structural, bool temporal*/)
        : declvisitor(dv)/*, enabled_structural(structural), enabled_temporal(temporal)*/ {}

    StatInfASTExtendExecInfoStmt_ns::STATUS Visit(Stmt *S);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitExpr(Expr *S);

    #define ABSTRACT_STMT(CLASS)
#define STMT(CLASS, PARENT) \
    StatInfASTExtendExecInfoStmt_ns::STATUS Visit##CLASS(CLASS *Node);
#include "clang/AST/StmtNodes.inc"

  StatInfASTExtendExecInfoStmt_ns::STATUS VisitCallArgs(CallExpr *Call);

  protected:
    StatInfASTExtendExecInfoStmt_ns::STATUS visitAllLoopKind(Stmt *loop, Stmt *loop_body);
  };

} // namespace
