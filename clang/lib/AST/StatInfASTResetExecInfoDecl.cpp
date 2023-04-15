//===--- StatInfASTResetExecInfoDecl.cpp - Printing implementation for Decl ASTs ----------===//
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
#include "clang/AST/StatInfASTResetExec.h"

#include <math.h>
#include <iostream>

namespace clang {

void StatInfASTResetExecInfoDecl::VisitFunctionDecl(FunctionDecl *F) {
    F->clearEntryTimestamp();
    F->clearExitTimestamp();
    if(F->getBody()) {
        StatInfASTResetExecInfoStmt stmtVisitor;
        stmtVisitor.Visit(F->getBody());
    }
}

void StatInfASTResetExecInfoStmt::Visit(Stmt *S) {
    S->clearExecCount();
    return StmtVisitor<StatInfASTResetExecInfoStmt>::Visit(S);
}

void StatInfASTResetExecInfoStmt::VisitCompoundStmt(CompoundStmt *node) {
    for (auto *I : node->body()) {
        Visit(I);
    }
}

void StatInfASTResetExecInfoStmt::VisitIfStmt(IfStmt *If) {
    if (If->getInit())
      Visit(If->getInit());
    if (const DeclStmt *DS = If->getConditionVariableDeclStmt()) {
      Visit(const_cast<DeclStmt*>(DS));
    }
    else
      Visit(If->getCond());
    Visit(If->getThen());
    if(If->getElse())
       Visit(If->getElse());
}

void StatInfASTResetExecInfoStmt::VisitForStmt(ForStmt *loop) {
  if (loop->getInit())
    Visit(loop->getInit());
  if (loop->getCond())
    Visit(loop->getCond());
  if (loop->getInc()) {
    Visit(loop->getInc());
  }
  Visit(loop->getBody());
}
void StatInfASTResetExecInfoStmt::VisitWhileStmt(WhileStmt *loop) {
  if (const DeclStmt *DS = loop->getConditionVariableDeclStmt()) {
    Visit(const_cast<DeclStmt*>(DS));
  }
  else
    Visit(loop->getCond());
  Visit(loop->getBody());
}
void StatInfASTResetExecInfoStmt::VisitDoStmt(DoStmt *loop) {
  Visit(loop->getCond());
  Visit(loop->getBody());
}

void StatInfASTResetExecInfoStmt::VisitSwitchStmt(SwitchStmt *Node) {
    if (Node->getInit())
      Visit(Node->getInit());
    if (const DeclStmt *DS = Node->getConditionVariableDeclStmt()) {
      Visit(const_cast<DeclStmt*>(DS));
    }
    else 
      Visit(Node->getCond());

    Visit(Node->getBody());
}

void StatInfASTResetExecInfoStmt::VisitCallExpr(CallExpr *call) {
    VisitCallArgs(call);
}

void StatInfASTResetExecInfoStmt::VisitCaseStmt(CaseStmt *Node) {
  Visit(Node->getSubStmt());
}

void StatInfASTResetExecInfoStmt::VisitDefaultStmt(DefaultStmt *Node) {
  Visit(Node->getSubStmt());
}

void StatInfASTResetExecInfoStmt::VisitLabelStmt(LabelStmt *Node) {
  Visit(Node->getSubStmt());
}

void StatInfASTResetExecInfoStmt::VisitAttributedStmt(AttributedStmt *Node) {
  Visit(Node->getSubStmt());
}

void StatInfASTResetExecInfoStmt::VisitReturnStmt(ReturnStmt *Node) {
  Visit(Node->getRetValue());
}

void StatInfASTResetExecInfoStmt::VisitMSDependentExistsStmt(MSDependentExistsStmt *Node) {
  Visit(Node->getSubStmt());
}

void StatInfASTResetExecInfoStmt::VisitGotoStmt(GotoStmt *Node) {
  llvm_unreachable("GOTO forbidden");
}

void StatInfASTResetExecInfoStmt::VisitIndirectGotoStmt(IndirectGotoStmt *Node) {
  llvm_unreachable("GOTO forbidden");
}

void StatInfASTResetExecInfoStmt::VisitGCCAsmStmt(GCCAsmStmt *Node) {
  for (unsigned i = 0, e = Node->getNumOutputs(); i != e; ++i) {
    Visit(Node->getOutputExpr(i));
  }

  for (unsigned i = 0, e = Node->getNumInputs(); i != e; ++i) {
    Visit(Node->getInputExpr(i));
  }
}

void StatInfASTResetExecInfoStmt::VisitCapturedStmt(CapturedStmt *Node) {
  Visit(Node->getCapturedDecl()->getBody());
}

void StatInfASTResetExecInfoStmt::VisitObjCAtTryStmt(ObjCAtTryStmt *Node) {
  if (auto *TS = dyn_cast<CompoundStmt>(Node->getTryBody())) {
    Visit(TS);
  }

  for (ObjCAtCatchStmt *catchStmt : Node->catch_stmts()) {
    if (auto *CS = dyn_cast<CompoundStmt>(catchStmt->getCatchBody())) {
      Visit(CS);
    }
  }

  if (auto *FS = static_cast<ObjCAtFinallyStmt *>(Node->getFinallyStmt())) {
    Visit(dyn_cast<CompoundStmt>(FS->getFinallyBody()));
  }
}

void StatInfASTResetExecInfoStmt::VisitObjCAtThrowStmt(ObjCAtThrowStmt *Node) {
  if (Node->getThrowExpr()) {
    Visit(Node->getThrowExpr());
  }
}

void StatInfASTResetExecInfoStmt::VisitObjCAtSynchronizedStmt(ObjCAtSynchronizedStmt *Node) {
  Visit(Node->getSynchExpr());
  Visit(Node->getSynchBody());
}

void StatInfASTResetExecInfoStmt::VisitObjCAutoreleasePoolStmt(ObjCAutoreleasePoolStmt *Node) {
  Visit(dyn_cast<CompoundStmt>(Node->getSubStmt()));
}

//===----------------------------------------------------------------------===//
//  Expr printing methods.
//===----------------------------------------------------------------------===//

void StatInfASTResetExecInfoStmt::VisitConstantExpr(ConstantExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitObjCIvarRefExpr(ObjCIvarRefExpr *Node) {
  if (Node->getBase()) {
      Visit(Node->getBase());
  }
}

void StatInfASTResetExecInfoStmt::VisitObjCSubscriptRefExpr(ObjCSubscriptRefExpr *Node) {
  Visit(Node->getBaseExpr());
  Visit(Node->getKeyExpr());
}

void StatInfASTResetExecInfoStmt::VisitImaginaryLiteral(ImaginaryLiteral *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitParenExpr(ParenExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitUnaryOperator(UnaryOperator *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitOffsetOfExpr(OffsetOfExpr *Node) {
  for (unsigned i = 0, n = Node->getNumComponents(); i < n; ++i) {
    OffsetOfNode ON = Node->getComponent(i);
    if (ON.getKind() == OffsetOfNode::Array) {
      Visit(Node->getIndexExpr(ON.getArrayExprIndex()));
    }
  }
}

void StatInfASTResetExecInfoStmt::VisitUnaryExprOrTypeTraitExpr(
    UnaryExprOrTypeTraitExpr *Node) {
  if (!Node->isArgumentType()) {
    Visit(Node->getArgumentExpr());
  }
}

void StatInfASTResetExecInfoStmt::VisitGenericSelectionExpr(GenericSelectionExpr *Node) {
  Visit(Node->getControllingExpr());
  for (const GenericSelectionExpr::Association Assoc : Node->associations()) {
    Visit(Assoc.getAssociationExpr());
  }
}

void StatInfASTResetExecInfoStmt::VisitArraySubscriptExpr(ArraySubscriptExpr *Node) {
  Visit(Node->getLHS());
  Visit(Node->getRHS());
}

void StatInfASTResetExecInfoStmt::VisitMatrixSubscriptExpr(MatrixSubscriptExpr *Node) {
  Visit(Node->getBase());
  Visit(Node->getRowIdx());
  Visit(Node->getColumnIdx());
}

void StatInfASTResetExecInfoStmt::VisitOMPArraySectionExpr(OMPArraySectionExpr *Node) {
  Visit(Node->getBase());
  if (Node->getLowerBound())
    Visit(Node->getLowerBound());
  if (Node->getColonLocFirst().isValid()) {
    if (Node->getLength())
      Visit(Node->getLength());
  }
  if (Node->getColonLocSecond().isValid()) {
    if (Node->getStride())
      Visit(Node->getStride());
  }
}

void StatInfASTResetExecInfoStmt::VisitOMPArrayShapingExpr(OMPArrayShapingExpr *Node) {
  for (Expr *E : Node->getDimensions()) {
    Visit(E);
  }
  Visit(Node->getBase());
}

void StatInfASTResetExecInfoStmt::VisitOMPIteratorExpr(OMPIteratorExpr *Node) {
  for (unsigned I = 0, E = Node->numOfIterators(); I < E; ++I) {
    const OMPIteratorExpr::IteratorRange Range = Node->getIteratorRange(I);
    Visit(Range.Begin);
    Visit(Range.End);
    if (Range.Step) {
      Visit(Range.Step);
    }
  }
}

void StatInfASTResetExecInfoStmt::VisitCallArgs(CallExpr *Call) {
  for (unsigned i = 0, e = Call->getNumArgs(); i != e; ++i) {
    Visit(Call->getArg(i));
  }
}

void StatInfASTResetExecInfoStmt::VisitMemberExpr(MemberExpr *Node) {
  Visit(Node->getBase());
}

void StatInfASTResetExecInfoStmt::VisitObjCIsaExpr(ObjCIsaExpr *Node) {
  Visit(Node->getBase());
}

void StatInfASTResetExecInfoStmt::VisitExtVectorElementExpr(ExtVectorElementExpr *Node) {
  Visit(Node->getBase());
}

void StatInfASTResetExecInfoStmt::VisitCStyleCastExpr(CStyleCastExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitCompoundLiteralExpr(CompoundLiteralExpr *Node) {
  Visit(Node->getInitializer());
}

void StatInfASTResetExecInfoStmt::VisitImplicitCastExpr(ImplicitCastExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitBinaryOperator(BinaryOperator *Node) {
  Visit(Node->getLHS());
  Visit(Node->getRHS());
}

void StatInfASTResetExecInfoStmt::VisitCompoundAssignOperator(CompoundAssignOperator *Node) {
  Visit(Node->getLHS());
  Visit(Node->getRHS());
}

void StatInfASTResetExecInfoStmt::VisitConditionalOperator(ConditionalOperator *Node) {
  Visit(Node->getCond());
  Visit(Node->getLHS());
  Visit(Node->getRHS());
}

// GNU extensions.

void StatInfASTResetExecInfoStmt::VisitBinaryConditionalOperator(BinaryConditionalOperator *Node) {
  Visit(Node->getCommon());
  Visit(Node->getFalseExpr());
}

void StatInfASTResetExecInfoStmt::VisitStmtExpr(StmtExpr *E) {
  Visit(E->getSubStmt());
}

void StatInfASTResetExecInfoStmt::VisitChooseExpr(ChooseExpr *Node) {
  Visit(Node->getCond());
  Visit(Node->getLHS());
  Visit(Node->getRHS());
}

void StatInfASTResetExecInfoStmt::VisitShuffleVectorExpr(ShuffleVectorExpr *Node) {
  for (unsigned i = 0, e = Node->getNumSubExprs(); i != e; ++i) {
    Visit(Node->getExpr(i));
  }
}

void StatInfASTResetExecInfoStmt::VisitConvertVectorExpr(ConvertVectorExpr *Node) {
  Visit(Node->getSrcExpr());
}

void StatInfASTResetExecInfoStmt::VisitInitListExpr(InitListExpr* Node) {
  if (Node->getSyntacticForm()) {
    Visit(Node->getSyntacticForm());
  }

  for (unsigned i = 0, e = Node->getNumInits(); i != e; ++i) {
    if (Node->getInit(i))
      Visit(Node->getInit(i));
  }
  
}

void StatInfASTResetExecInfoStmt::VisitArrayInitLoopExpr(ArrayInitLoopExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitParenListExpr(ParenListExpr* Node) {
  for (unsigned i = 0, e = Node->getNumExprs(); i != e; ++i) {
    Visit(Node->getExpr(i));
  }
  
}

void StatInfASTResetExecInfoStmt::VisitDesignatedInitExpr(DesignatedInitExpr *Node) {
  for (const DesignatedInitExpr::Designator &D : Node->designators()) {
    if (!D.isFieldDesignator()) {
      if (D.isArrayDesignator()) {
        Visit(Node->getArrayIndex(D));
      } else {
        Visit(Node->getArrayRangeStart(D));
        Visit(Node->getArrayRangeEnd(D));
      }
    }
  }
  Visit(Node->getInit());
}

void StatInfASTResetExecInfoStmt::VisitDesignatedInitUpdateExpr(
    DesignatedInitUpdateExpr *Node) {
  Visit(Node->getBase());
  Visit(Node->getUpdater());
}

void StatInfASTResetExecInfoStmt::VisitVAArgExpr(VAArgExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitPseudoObjectExpr(PseudoObjectExpr *Node) {
  Visit(Node->getSyntacticForm());
}

void StatInfASTResetExecInfoStmt::VisitAtomicExpr(AtomicExpr *Node) {
  // AtomicExpr stores its subexpressions in a permuted order.
  Visit(Node->getPtr());
  if (Node->getOp() != AtomicExpr::AO__c11_atomic_load &&
      Node->getOp() != AtomicExpr::AO__atomic_load_n &&
      Node->getOp() != AtomicExpr::AO__opencl_atomic_load &&
      Node->getOp() != AtomicExpr::AO__hip_atomic_load) {
    Visit(Node->getVal1());
  }
  if (Node->getOp() == AtomicExpr::AO__atomic_exchange ||
      Node->isCmpXChg()) {
    Visit(Node->getVal2());
  }
  if (Node->getOp() == AtomicExpr::AO__atomic_compare_exchange ||
      Node->getOp() == AtomicExpr::AO__atomic_compare_exchange_n) {
    Visit(Node->getWeak());
  }
  if (Node->getOp() != AtomicExpr::AO__c11_atomic_init &&
      Node->getOp() != AtomicExpr::AO__opencl_atomic_init) {
    Visit(Node->getOrder());
  }
  if (Node->isCmpXChg()) {
    Visit(Node->getOrderFail());
  }
  
}

// C++
void StatInfASTResetExecInfoStmt::VisitCXXOperatorCallExpr(CXXOperatorCallExpr *Node) {
  OverloadedOperatorKind Kind = Node->getOperator();
  if (Kind == OO_PlusPlus || Kind == OO_MinusMinus) {
      Visit(Node->getArg(0));
  } 
  else if (Kind == OO_Arrow) {
    Visit(Node->getArg(0));
  } 
  else if (Kind == OO_Call || Kind == OO_Subscript) {
    Visit(Node->getArg(0));
    for (unsigned ArgIdx = 1; ArgIdx < Node->getNumArgs(); ++ArgIdx) {
        Visit(Node->getArg(ArgIdx));
    }
  } 
  else if (Node->getNumArgs() == 1) {
    Visit(Node->getArg(0));
  } 
  else if (Node->getNumArgs() == 2) {
    Visit(Node->getArg(0));
    Visit(Node->getArg(1));
  } 
  else {
    llvm_unreachable("unknown overloaded operator");
  }
  
}

void StatInfASTResetExecInfoStmt::VisitCXXMemberCallExpr(CXXMemberCallExpr *Node) {
  CXXMethodDecl *MD = Node->getMethodDecl();
  if (MD && isa<CXXConversionDecl>(MD)) {
    Visit(Node->getImplicitObjectArgument());
  }
  VisitCallExpr(cast<CallExpr>(Node));
}

void StatInfASTResetExecInfoStmt::VisitCUDAKernelCallExpr(CUDAKernelCallExpr *Node) {
  Visit(Node->getCallee());
  VisitCallArgs(Node->getConfig());
  VisitCallArgs(Node);
}

void StatInfASTResetExecInfoStmt::VisitCXXRewrittenBinaryOperator(
    CXXRewrittenBinaryOperator *Node) {
  CXXRewrittenBinaryOperator::DecomposedForm Decomposed =
      Node->getDecomposedForm();
  Visit(const_cast<Expr*>(Decomposed.LHS));
  Visit(const_cast<Expr*>(Decomposed.RHS));
}

void StatInfASTResetExecInfoStmt::VisitCXXNamedCastExpr(CXXNamedCastExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitCXXStaticCastExpr(CXXStaticCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfASTResetExecInfoStmt::VisitCXXDynamicCastExpr(CXXDynamicCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfASTResetExecInfoStmt::VisitCXXReinterpretCastExpr(CXXReinterpretCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfASTResetExecInfoStmt::VisitCXXConstCastExpr(CXXConstCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfASTResetExecInfoStmt::VisitBuiltinBitCastExpr(BuiltinBitCastExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitCXXAddrspaceCastExpr(CXXAddrspaceCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfASTResetExecInfoStmt::VisitCXXTypeidExpr(CXXTypeidExpr *Node) {
  if (!Node->isTypeOperand()) {
    Visit(Node->getExprOperand());
  }
  
}

void StatInfASTResetExecInfoStmt::VisitCXXUuidofExpr(CXXUuidofExpr *Node) {
  if (!Node->isTypeOperand()) {
    Visit(Node->getExprOperand());
  }
  
}

void StatInfASTResetExecInfoStmt::VisitMSPropertyRefExpr(MSPropertyRefExpr *Node) {
  Visit(Node->getBaseExpr());
}

void StatInfASTResetExecInfoStmt::VisitMSPropertySubscriptExpr(MSPropertySubscriptExpr *Node) {
  Visit(Node->getBase());
  Visit(Node->getIdx());
}

void StatInfASTResetExecInfoStmt::VisitUserDefinedLiteral(UserDefinedLiteral *Node) {
  switch (Node->getLiteralOperatorKind()) {
    case UserDefinedLiteral::LOK_Character:
      Visit(Node->getCookedLiteral());
      break;
    default:
      break;
  }
  
}

void StatInfASTResetExecInfoStmt::VisitCXXThrowExpr(CXXThrowExpr *Node) {
  if (Node->getSubExpr())
    Visit(Node->getSubExpr());
  
}

void StatInfASTResetExecInfoStmt::VisitCXXFunctionalCastExpr(CXXFunctionalCastExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *Node) {
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitCXXTemporaryObjectExpr(CXXTemporaryObjectExpr *Node) {
  for (CXXTemporaryObjectExpr::arg_iterator Arg = Node->arg_begin(),
                                         ArgEnd = Node->arg_end();
       Arg != ArgEnd; ++Arg) {
    Visit(*Arg);
  }
  
}

void StatInfASTResetExecInfoStmt::VisitLambdaExpr(LambdaExpr *Node) {
  for (LambdaExpr::capture_iterator C = Node->explicit_capture_begin(),
                                 CEnd = Node->explicit_capture_end();
       C != CEnd;
       ++C) {
    if (Node->isInitCapture(C)) {
      VarDecl *D = C->getCapturedVar();
      Visit(D->getInit());
    }
  }

  Visit(Node->getCompoundStmtBody());
}

void StatInfASTResetExecInfoStmt::VisitCXXNewExpr(CXXNewExpr *E) {
  unsigned NumPlace = E->getNumPlacementArgs();
  if (NumPlace > 0 && !isa<CXXDefaultArgExpr>(E->getPlacementArg(0))) {
    Visit(E->getPlacementArg(0));
    for (unsigned i = 1; i < NumPlace; ++i) {
      Visit(E->getPlacementArg(i));
    }
  }

  CXXNewExpr::InitializationStyle InitStyle = E->getInitializationStyle();
  if (InitStyle != CXXNewExpr::NoInit) {
    Visit(E->getInitializer());
  }
  
}

void StatInfASTResetExecInfoStmt::VisitCXXDeleteExpr(CXXDeleteExpr *E) {
  Visit(E->getArgument());
}

void StatInfASTResetExecInfoStmt::VisitCXXPseudoDestructorExpr(CXXPseudoDestructorExpr *E) {
  Visit(E->getBase());
}

void StatInfASTResetExecInfoStmt::VisitCXXConstructExpr(CXXConstructExpr *E) {
  for (unsigned i = 0, e = E->getNumArgs(); i != e; ++i) {
    Visit(E->getArg(i));
  }
  
}

void StatInfASTResetExecInfoStmt::VisitCXXStdInitializerListExpr(CXXStdInitializerListExpr *E) {
  Visit(E->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitExprWithCleanups(ExprWithCleanups *E) {
  Visit(E->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitCXXUnresolvedConstructExpr(
    CXXUnresolvedConstructExpr *Node) {
  for (auto Arg = Node->arg_begin(), ArgEnd = Node->arg_end(); Arg != ArgEnd;
       ++Arg) {
    Visit(*Arg);
  }
  
}

void StatInfASTResetExecInfoStmt::VisitCXXDependentScopeMemberExpr(
                                         CXXDependentScopeMemberExpr *Node) {
  if (!Node->isImplicitAccess()) {
    Visit(Node->getBase());
  }
  
}

void StatInfASTResetExecInfoStmt::VisitUnresolvedMemberExpr(UnresolvedMemberExpr *Node) {
  if (!Node->isImplicitAccess()) {
    Visit(Node->getBase());
  }
  
}

void StatInfASTResetExecInfoStmt::VisitExpressionTraitExpr(ExpressionTraitExpr *E) {
  Visit(E->getQueriedExpression());
}

void StatInfASTResetExecInfoStmt::VisitCXXNoexceptExpr(CXXNoexceptExpr *E) {
  Visit(E->getOperand());
}

void StatInfASTResetExecInfoStmt::VisitPackExpansionExpr(PackExpansionExpr *E) {
  Visit(E->getPattern());
}

void StatInfASTResetExecInfoStmt::VisitSubstNonTypeTemplateParmExpr(
                                       SubstNonTypeTemplateParmExpr *Node) {
  Visit(Node->getReplacement());
}

void StatInfASTResetExecInfoStmt::VisitMaterializeTemporaryExpr(MaterializeTemporaryExpr *Node){
  Visit(Node->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitCXXFoldExpr(CXXFoldExpr *E) {
  if (E->getLHS()) {
    Visit(E->getLHS());
  }
  if (E->getRHS()) {
    Visit(E->getRHS());
  }
  
}

void StatInfASTResetExecInfoStmt::VisitRequiresExpr(RequiresExpr *E) {
  auto Requirements = E->getRequirements();
  for (concepts::Requirement *Req : Requirements) {
    if (auto *ExprReq = dyn_cast<concepts::ExprRequirement>(Req)) {
      if (!ExprReq->isExprSubstitutionFailure())
        Visit(ExprReq->getExpr());
    } 
    else {
      auto *NestedReq = cast<concepts::NestedRequirement>(Req);
      if (!NestedReq->isSubstitutionFailure())
        Visit(NestedReq->getConstraintExpr());
    }
  }
  
}

// C++ Coroutines TS

void StatInfASTResetExecInfoStmt::VisitCoroutineBodyStmt(CoroutineBodyStmt *S) {
  Visit(S->getBody());
}

void StatInfASTResetExecInfoStmt::VisitCoreturnStmt(CoreturnStmt *S) {
  if (S->getOperand()) {
    Visit(S->getOperand());
  }
  
}

void StatInfASTResetExecInfoStmt::VisitCoawaitExpr(CoawaitExpr *S) {
  Visit(S->getOperand());
}

void StatInfASTResetExecInfoStmt::VisitDependentCoawaitExpr(DependentCoawaitExpr *S) {
  Visit(S->getOperand());
}

void StatInfASTResetExecInfoStmt::VisitCoyieldExpr(CoyieldExpr *S) {
  Visit(S->getOperand());
}

// Obj-C

void StatInfASTResetExecInfoStmt::VisitObjCBoxedExpr(ObjCBoxedExpr *E) {
  Visit(E->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitObjCArrayLiteral(ObjCArrayLiteral *E) {
  ObjCArrayLiteral::child_range Ch = E->children();
  for (auto I = Ch.begin(), E = Ch.end(); I != E; ++I) {
    Visit(*I);
  }
  
}

void StatInfASTResetExecInfoStmt::VisitObjCDictionaryLiteral(ObjCDictionaryLiteral *E) {
  for (unsigned I = 0, N = E->getNumElements(); I != N; ++I) {
    ObjCDictionaryElement Element = E->getKeyValueElement(I);
    Visit(Element.Key);
    Visit(Element.Value);
  }
  
}

void StatInfASTResetExecInfoStmt::VisitObjCMessageExpr(ObjCMessageExpr *Mess) {
  switch (Mess->getReceiverKind()) {
  case ObjCMessageExpr::Instance:
    Visit(Mess->getInstanceReceiver());
    break;
  default:
    break;
  }

  Selector selector = Mess->getSelector();
  if (!selector.isUnarySelector()) {
    for (unsigned i = 0, e = Mess->getNumArgs(); i != e; ++i) {
      Visit(Mess->getArg(i));
    }
  }
  
}

void StatInfASTResetExecInfoStmt::VisitObjCIndirectCopyRestoreExpr(ObjCIndirectCopyRestoreExpr *E) {
  Visit(E->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitObjCBridgedCastExpr(ObjCBridgedCastExpr *E) {
  Visit(E->getSubExpr());
}

void StatInfASTResetExecInfoStmt::VisitOpaqueValueExpr(OpaqueValueExpr *Node) {
  Visit(Node->getSourceExpr());
}

void StatInfASTResetExecInfoStmt::VisitTypoExpr(TypoExpr *Node) {
  // TODO: Print something reasonable for a TypoExpr, if necessary.
  llvm_unreachable("Cannot print TypoExpr nodes");
}

void StatInfASTResetExecInfoStmt::VisitRecoveryExpr(RecoveryExpr *Node) {
  for (Expr *E : Node->subExpressions()) {
    Visit(E);
  }
  
}

void StatInfASTResetExecInfoStmt::VisitAsTypeExpr(AsTypeExpr *Node) {
  Visit(Node->getSrcExpr());
}

}