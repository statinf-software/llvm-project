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

    bool enabled_structural;
    bool enabled_temporal;

  public:
    StatInfASTExtendExecInfoStmt(StatInfASTExtendExecInfoDecl *dv, bool structural, bool temporal)
        : declvisitor(dv), enabled_structural(structural), enabled_temporal(temporal) {}

    StatInfASTExtendExecInfoStmt_ns::STATUS Visit(Stmt *S);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitIfStmt(IfStmt *If);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitSwitchStmt(SwitchStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitWhileStmt(WhileStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitDoStmt(DoStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitForStmt(ForStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCallExpr(CallExpr *Call);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCompoundStmt(CompoundStmt *node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCaseStmt(CaseStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitDefaultStmt(DefaultStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitLabelStmt(LabelStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitAttributedStmt(AttributedStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitReturnStmt(ReturnStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitContinueStmt(ContinueStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitBreakStmt(BreakStmt *Node);
    
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitMSDependentExistsStmt(MSDependentExistsStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitGotoStmt(GotoStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitIndirectGotoStmt(IndirectGotoStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitGCCAsmStmt(GCCAsmStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCapturedStmt(CapturedStmt *Node) ;
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCAtTryStmt(ObjCAtTryStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCAtThrowStmt(ObjCAtThrowStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCAtSynchronizedStmt(ObjCAtSynchronizedStmt *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCAutoreleasePoolStmt(ObjCAutoreleasePoolStmt *Node);

    //===----------------------------------------------------------------------===//
    //  Expr printing methods.
    //===----------------------------------------------------------------------===//
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitConstantExpr(ConstantExpr *Node) ;
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCIvarRefExpr(ObjCIvarRefExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCSubscriptRefExpr(ObjCSubscriptRefExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitImaginaryLiteral(ImaginaryLiteral *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitParenExpr(ParenExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitUnaryOperator(UnaryOperator *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitOffsetOfExpr(OffsetOfExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitGenericSelectionExpr(GenericSelectionExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitArraySubscriptExpr(ArraySubscriptExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitMatrixSubscriptExpr(MatrixSubscriptExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitOMPArraySectionExpr(OMPArraySectionExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitOMPArrayShapingExpr(OMPArrayShapingExpr *Node) ;
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitOMPIteratorExpr(OMPIteratorExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCallArgs(CallExpr *Call) ;
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitMemberExpr(MemberExpr *Node) ;
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCIsaExpr(ObjCIsaExpr *Node) ;
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitExtVectorElementExpr(ExtVectorElementExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCStyleCastExpr(CStyleCastExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCompoundLiteralExpr(CompoundLiteralExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitImplicitCastExpr(ImplicitCastExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitBinaryOperator(BinaryOperator *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCompoundAssignOperator(CompoundAssignOperator *Node) ;
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitConditionalOperator(ConditionalOperator *Node);

    // GNU extensions.
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitBinaryConditionalOperator(BinaryConditionalOperator *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitStmtExpr(StmtExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitChooseExpr(ChooseExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitShuffleVectorExpr(ShuffleVectorExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitConvertVectorExpr(ConvertVectorExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitInitListExpr(InitListExpr* Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitArrayInitLoopExpr(ArrayInitLoopExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitParenListExpr(ParenListExpr* Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitDesignatedInitExpr(DesignatedInitExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitDesignatedInitUpdateExpr(DesignatedInitUpdateExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitVAArgExpr(VAArgExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitPseudoObjectExpr(PseudoObjectExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitAtomicExpr(AtomicExpr *Node);

    // C++
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXOperatorCallExpr(CXXOperatorCallExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXMemberCallExpr(CXXMemberCallExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCUDAKernelCallExpr(CUDAKernelCallExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXRewrittenBinaryOperator(CXXRewrittenBinaryOperator *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXNamedCastExpr(CXXNamedCastExpr *Node) ;
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXStaticCastExpr(CXXStaticCastExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXDynamicCastExpr(CXXDynamicCastExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXReinterpretCastExpr(CXXReinterpretCastExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXConstCastExpr(CXXConstCastExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitBuiltinBitCastExpr(BuiltinBitCastExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXAddrspaceCastExpr(CXXAddrspaceCastExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXTypeidExpr(CXXTypeidExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXUuidofExpr(CXXUuidofExpr *Node) ;
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitMSPropertyRefExpr(MSPropertyRefExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitMSPropertySubscriptExpr(MSPropertySubscriptExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitUserDefinedLiteral(UserDefinedLiteral *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXThrowExpr(CXXThrowExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXFunctionalCastExpr(CXXFunctionalCastExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXTemporaryObjectExpr(CXXTemporaryObjectExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitLambdaExpr(LambdaExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXNewExpr(CXXNewExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXDeleteExpr(CXXDeleteExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXPseudoDestructorExpr(CXXPseudoDestructorExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXConstructExpr(CXXConstructExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXStdInitializerListExpr(CXXStdInitializerListExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitExprWithCleanups(ExprWithCleanups *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXUnresolvedConstructExpr(CXXUnresolvedConstructExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXDependentScopeMemberExpr(CXXDependentScopeMemberExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitUnresolvedMemberExpr(UnresolvedMemberExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitExpressionTraitExpr(ExpressionTraitExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXNoexceptExpr(CXXNoexceptExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitPackExpansionExpr(PackExpansionExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitSubstNonTypeTemplateParmExpr(SubstNonTypeTemplateParmExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitMaterializeTemporaryExpr(MaterializeTemporaryExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCXXFoldExpr(CXXFoldExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitRequiresExpr(RequiresExpr *E);

    // C++ Coroutines TS
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCoroutineBodyStmt(CoroutineBodyStmt *S);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCoreturnStmt(CoreturnStmt *S);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCoawaitExpr(CoawaitExpr *S);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitDependentCoawaitExpr(DependentCoawaitExpr *S);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitCoyieldExpr(CoyieldExpr *S);

    // Obj-C
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCBoxedExpr(ObjCBoxedExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCArrayLiteral(ObjCArrayLiteral *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCDictionaryLiteral(ObjCDictionaryLiteral *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCMessageExpr(ObjCMessageExpr *Mess);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCIndirectCopyRestoreExpr(ObjCIndirectCopyRestoreExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitObjCBridgedCastExpr(ObjCBridgedCastExpr *E);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitOpaqueValueExpr(OpaqueValueExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitTypoExpr(TypoExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitRecoveryExpr(RecoveryExpr *Node);
    StatInfASTExtendExecInfoStmt_ns::STATUS VisitAsTypeExpr(AsTypeExpr *Node);

  protected:
    StatInfASTExtendExecInfoStmt_ns::STATUS visitAllLoopKind(Stmt *loop, Stmt *loop_body);
  };

} // namespace
