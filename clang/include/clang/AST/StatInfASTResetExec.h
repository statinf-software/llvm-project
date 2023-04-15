//===--- DeclPrinter.cpp - Printing implementation for Decl ASTs ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the Decl::print method, which pretty prints the
// AST back out to C/Objective-C/C++/Objective-C++ code.
//
//===----------------------------------------------------------------------===//
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/Basic/Module.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/AST/DeclOpenMP.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/ExprOpenMP.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/OpenMPClause.h"
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

#include <cassert>
#include <optional>
#include <string>

namespace clang {
  class StatInfASTResetExecInfoDecl : public DeclVisitor<StatInfASTResetExecInfoDecl> {
    const ASTContext &Context;

  public:
    StatInfASTResetExecInfoDecl(const ASTContext &Context)
        : Context(Context) {}

    void VisitFunctionDecl(FunctionDecl *F);
  };

  class StatInfASTResetExecInfoStmt : public StmtVisitor<StatInfASTResetExecInfoStmt> {
  public:
    StatInfASTResetExecInfoStmt() {}

    void Visit(Stmt *S);
    void VisitIfStmt(IfStmt *If);
    void VisitSwitchStmt(SwitchStmt *Node);
    void VisitWhileStmt(WhileStmt *Node);
    void VisitDoStmt(DoStmt *Node);
    void VisitForStmt(ForStmt *Node);
    void VisitCallExpr(CallExpr *Call);
    void VisitCompoundStmt(CompoundStmt *node);
    void VisitCaseStmt(CaseStmt *Node);
    void VisitDefaultStmt(DefaultStmt *Node);
    void VisitLabelStmt(LabelStmt *Node);
    void VisitAttributedStmt(AttributedStmt *Node);
    void VisitReturnStmt(ReturnStmt *Node);
    
    void VisitMSDependentExistsStmt(MSDependentExistsStmt *Node);
    void VisitGotoStmt(GotoStmt *Node);
    void VisitIndirectGotoStmt(IndirectGotoStmt *Node);
    void VisitGCCAsmStmt(GCCAsmStmt *Node);
    void VisitCapturedStmt(CapturedStmt *Node) ;
    void VisitObjCAtTryStmt(ObjCAtTryStmt *Node);
    void VisitObjCAtThrowStmt(ObjCAtThrowStmt *Node);
    void VisitObjCAtSynchronizedStmt(ObjCAtSynchronizedStmt *Node);
    void VisitObjCAutoreleasePoolStmt(ObjCAutoreleasePoolStmt *Node);

    //===----------------------------------------------------------------------===//
    //  Expr printing methods.
    //===----------------------------------------------------------------------===//
    void VisitConstantExpr(ConstantExpr *Node) ;
    void VisitObjCIvarRefExpr(ObjCIvarRefExpr *Node);
    void VisitObjCSubscriptRefExpr(ObjCSubscriptRefExpr *Node);
    void VisitImaginaryLiteral(ImaginaryLiteral *Node);
    void VisitParenExpr(ParenExpr *Node);
    void VisitUnaryOperator(UnaryOperator *Node);
    void VisitOffsetOfExpr(OffsetOfExpr *Node);
    void VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *Node);
    void VisitGenericSelectionExpr(GenericSelectionExpr *Node);
    void VisitArraySubscriptExpr(ArraySubscriptExpr *Node);
    void VisitMatrixSubscriptExpr(MatrixSubscriptExpr *Node);
    void VisitOMPArraySectionExpr(OMPArraySectionExpr *Node);
    void VisitOMPArrayShapingExpr(OMPArrayShapingExpr *Node) ;
    void VisitOMPIteratorExpr(OMPIteratorExpr *Node);
    void VisitCallArgs(CallExpr *Call) ;
    void VisitMemberExpr(MemberExpr *Node) ;
    void VisitObjCIsaExpr(ObjCIsaExpr *Node) ;
    void VisitExtVectorElementExpr(ExtVectorElementExpr *Node);
    void VisitCStyleCastExpr(CStyleCastExpr *Node);
    void VisitCompoundLiteralExpr(CompoundLiteralExpr *Node);
    void VisitImplicitCastExpr(ImplicitCastExpr *Node);
    void VisitBinaryOperator(BinaryOperator *Node);
    void VisitCompoundAssignOperator(CompoundAssignOperator *Node) ;
    void VisitConditionalOperator(ConditionalOperator *Node);

    // GNU extensions.
    void VisitBinaryConditionalOperator(BinaryConditionalOperator *Node);
    void VisitStmtExpr(StmtExpr *E);
    void VisitChooseExpr(ChooseExpr *Node);
    void VisitShuffleVectorExpr(ShuffleVectorExpr *Node);
    void VisitConvertVectorExpr(ConvertVectorExpr *Node);
    void VisitInitListExpr(InitListExpr* Node);
    void VisitArrayInitLoopExpr(ArrayInitLoopExpr *Node);
    void VisitParenListExpr(ParenListExpr* Node);
    void VisitDesignatedInitExpr(DesignatedInitExpr *Node);
    void VisitDesignatedInitUpdateExpr(DesignatedInitUpdateExpr *Node);
    void VisitVAArgExpr(VAArgExpr *Node);
    void VisitPseudoObjectExpr(PseudoObjectExpr *Node);
    void VisitAtomicExpr(AtomicExpr *Node);

    // C++
    void VisitCXXOperatorCallExpr(CXXOperatorCallExpr *Node);
    void VisitCXXMemberCallExpr(CXXMemberCallExpr *Node);
    void VisitCUDAKernelCallExpr(CUDAKernelCallExpr *Node);
    void VisitCXXRewrittenBinaryOperator(CXXRewrittenBinaryOperator *Node);
    void VisitCXXNamedCastExpr(CXXNamedCastExpr *Node) ;
    void VisitCXXStaticCastExpr(CXXStaticCastExpr *Node);
    void VisitCXXDynamicCastExpr(CXXDynamicCastExpr *Node);
    void VisitCXXReinterpretCastExpr(CXXReinterpretCastExpr *Node);
    void VisitCXXConstCastExpr(CXXConstCastExpr *Node);
    void VisitBuiltinBitCastExpr(BuiltinBitCastExpr *Node);
    void VisitCXXAddrspaceCastExpr(CXXAddrspaceCastExpr *Node);
    void VisitCXXTypeidExpr(CXXTypeidExpr *Node);
    void VisitCXXUuidofExpr(CXXUuidofExpr *Node) ;
    void VisitMSPropertyRefExpr(MSPropertyRefExpr *Node);
    void VisitMSPropertySubscriptExpr(MSPropertySubscriptExpr *Node);
    void VisitUserDefinedLiteral(UserDefinedLiteral *Node);
    void VisitCXXThrowExpr(CXXThrowExpr *Node);
    void VisitCXXFunctionalCastExpr(CXXFunctionalCastExpr *Node);
    void VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *Node);
    void VisitCXXTemporaryObjectExpr(CXXTemporaryObjectExpr *Node);
    void VisitLambdaExpr(LambdaExpr *Node);
    void VisitCXXNewExpr(CXXNewExpr *E);
    void VisitCXXDeleteExpr(CXXDeleteExpr *E);
    void VisitCXXPseudoDestructorExpr(CXXPseudoDestructorExpr *E);
    void VisitCXXConstructExpr(CXXConstructExpr *E);
    void VisitCXXStdInitializerListExpr(CXXStdInitializerListExpr *E);
    void VisitExprWithCleanups(ExprWithCleanups *E);
    void VisitCXXUnresolvedConstructExpr(CXXUnresolvedConstructExpr *Node);
    void VisitCXXDependentScopeMemberExpr(CXXDependentScopeMemberExpr *Node);
    void VisitUnresolvedMemberExpr(UnresolvedMemberExpr *Node);
    void VisitExpressionTraitExpr(ExpressionTraitExpr *E);
    void VisitCXXNoexceptExpr(CXXNoexceptExpr *E);
    void VisitPackExpansionExpr(PackExpansionExpr *E);
    void VisitSubstNonTypeTemplateParmExpr(SubstNonTypeTemplateParmExpr *Node);
    void VisitMaterializeTemporaryExpr(MaterializeTemporaryExpr *Node);
    void VisitCXXFoldExpr(CXXFoldExpr *E);
    void VisitRequiresExpr(RequiresExpr *E);

    // C++ Coroutines TS
    void VisitCoroutineBodyStmt(CoroutineBodyStmt *S);
    void VisitCoreturnStmt(CoreturnStmt *S);
    void VisitCoawaitExpr(CoawaitExpr *S);
    void VisitDependentCoawaitExpr(DependentCoawaitExpr *S);
    void VisitCoyieldExpr(CoyieldExpr *S);

    // Obj-C
    void VisitObjCBoxedExpr(ObjCBoxedExpr *E);
    void VisitObjCArrayLiteral(ObjCArrayLiteral *E);
    void VisitObjCDictionaryLiteral(ObjCDictionaryLiteral *E);
    void VisitObjCMessageExpr(ObjCMessageExpr *Mess);
    void VisitObjCIndirectCopyRestoreExpr(ObjCIndirectCopyRestoreExpr *E);
    void VisitObjCBridgedCastExpr(ObjCBridgedCastExpr *E);
    void VisitOpaqueValueExpr(OpaqueValueExpr *Node);
    void VisitTypoExpr(TypoExpr *Node);
    void VisitRecoveryExpr(RecoveryExpr *Node);
    void VisitAsTypeExpr(AsTypeExpr *Node);

  protected:
    void visitAllLoopKind(Stmt *loop, Stmt *loop_body);
  };
}
