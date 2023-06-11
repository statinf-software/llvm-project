//===- StatInfASTExtendExecInfoStmt.cpp - Printing implementation for Stmt ASTs ------------===//
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

#include "clang/AST/StatInfASTExtendExecInfoStmt.h"
#include "clang/AST/StatInfASTExtendExecInfoDecl.h"

#include <vector>

namespace clang {

#define check_if_proceed(X) {\
  StatInfASTExtendExecInfoStmt_ns::STATUS status = X;\
  if(status != StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED){ return status; }\
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::Visit(Stmt *S) {
  if(S) {
    S->incrExec();
    return StatInfASTExtendExecInfoStmtParent::Visit(S);
  }
  else
    return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitExpr(Expr *S) {
  if(S) {
    S->incrExec();
    return StatInfASTExtendExecInfoStmtParent::VisitExpr(S);
  }
  else
    return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCompoundStmt(CompoundStmt *node) {
    for (auto *I : node->body()) {
        StatInfASTExtendExecInfoStmt_ns::STATUS status = Visit(I);
        if(status != StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED)
            return status;
    }
    return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitIfStmt(IfStmt *If) {
    if (If->getInit())
      check_if_proceed(Visit(If->getInit()));
    if (const DeclStmt *DS = If->getConditionVariableDeclStmt()) {
      check_if_proceed(Visit(const_cast<DeclStmt*>(DS)));
    }
    else
      check_if_proceed(Visit(If->getCond()));

    uint8_t get_branch_taken=42;
    if(!declvisitor->bitstream()->getTraceBit(&get_branch_taken))
      return StatInfASTExtendExecInfoStmt_ns::STATUS::EXIT_FUNCTION;

    if(get_branch_taken) {
        return Visit(If->getThen());
    }
    else {
        if(If->getElse()) {
          return Visit(If->getElse());
        }
    }
    return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::visitAllLoopKind(Stmt *loop, Stmt *loop_body) {
    uint8_t get_branch_taken;
    if(!declvisitor->bitstream()->getTraceBit(&get_branch_taken))
      return StatInfASTExtendExecInfoStmt_ns::STATUS::EXIT_FUNCTION;
    while(get_branch_taken) {
        loop->incrExec();
        StatInfASTExtendExecInfoStmt_ns::STATUS status = Visit(loop_body);
        if(status == StatInfASTExtendExecInfoStmt_ns::STATUS::BREAK_LOOP)
            return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
        if(status == StatInfASTExtendExecInfoStmt_ns::STATUS::EXIT_FUNCTION)
            return status;

        if(!declvisitor->bitstream()->getTraceBit(&get_branch_taken))
          return StatInfASTExtendExecInfoStmt_ns::STATUS::EXIT_FUNCTION;
    }
    return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitForStmt(ForStmt *loop) {
  if (loop->getInit())
    check_if_proceed(Visit(loop->getInit()));
  if (loop->getCond())
    check_if_proceed(Visit(loop->getCond()));
  if (loop->getInc()) {
    check_if_proceed(Visit(loop->getInc()));
  }
    return visitAllLoopKind(loop, loop->getBody());
}
StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitWhileStmt(WhileStmt *loop) {
  if (const DeclStmt *DS = loop->getConditionVariableDeclStmt()) {
    check_if_proceed(Visit(const_cast<DeclStmt*>(DS)));
  }
  else
    check_if_proceed(Visit(loop->getCond()));
  return visitAllLoopKind(loop, loop->getBody());
}
StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitDoStmt(DoStmt *loop) {
  check_if_proceed(Visit(loop->getCond()));
  return visitAllLoopKind(loop, loop->getBody());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSwitchStmt(SwitchStmt *Node) {
    if (Node->getInit())
      check_if_proceed(Visit(Node->getInit()));
    if (const DeclStmt *DS = Node->getConditionVariableDeclStmt()) {
      check_if_proceed(Visit(const_cast<DeclStmt*>(DS)));
    }
    else 
      check_if_proceed(Visit(Node->getCond()));

    CompoundStmt *body = dyn_cast<CompoundStmt>(Node->getBody());
    std::vector<Stmt*> cases;
    if(body) {
      for(auto ch : body->body()) {
        if(isa<CaseStmt>(ch) || isa<DefaultStmt>(ch))
          cases.push_back(ch);
      }
    }
    else
      llvm::errs() << "Weird: Child of a Switch is not a CompoundStmt but a " << Node->getBody()->getStmtClassName();

    for(uint32_t cid=0 ; cid < cases.size() ; ++cid) {
        uint8_t is_case_taken;
        if(!declvisitor->bitstream()->getTraceBit(&is_case_taken))
          return StatInfASTExtendExecInfoStmt_ns::STATUS::EXIT_FUNCTION;
        if(is_case_taken) {
            StatInfASTExtendExecInfoStmt_ns::STATUS status = Visit(cases[cid]);
            if(status == StatInfASTExtendExecInfoStmt_ns::STATUS::BREAK_LOOP) {
                declvisitor->bitstream()->consumeNbits(cases.size()-cid);
                return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
            }
            if(status == StatInfASTExtendExecInfoStmt_ns::STATUS::EXIT_FUNCTION) {
                declvisitor->bitstream()->consumeNbits(cases.size()-cid);
                return status;
            }

        }
    }
    
    return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCallExpr(CallExpr *call) {
    VisitCallArgs(call);
    if(call->getCalleeDecl())
      declvisitor->Visit(call->getCalleeDecl());
    //if the CalleDecl is null it most likely because it's an indirect call to an adress
    return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCaseStmt(CaseStmt *Node) {
  return Visit(Node->getSubStmt());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitDefaultStmt(DefaultStmt *Node) {
  return Visit(Node->getSubStmt());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitLabelStmt(LabelStmt *Node) {
  return Visit(Node->getSubStmt());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitAttributedStmt(AttributedStmt *Node) {
  return Visit(Node->getSubStmt());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitReturnStmt(ReturnStmt *Node) {
  Visit(Node->getRetValue());
  return StatInfASTExtendExecInfoStmt_ns::STATUS::EXIT_FUNCTION;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitContinueStmt(ContinueStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::CONTINUE_LOOP;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitBreakStmt(BreakStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::BREAK_LOOP;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitMSDependentExistsStmt(MSDependentExistsStmt *Node) {
  return Visit(Node->getSubStmt());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitGotoStmt(GotoStmt *Node) {
  llvm_unreachable("GOTO forbidden");
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitIndirectGotoStmt(IndirectGotoStmt *Node) {
  llvm_unreachable("GOTO forbidden");
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitGCCAsmStmt(GCCAsmStmt *Node) {
  for (unsigned i = 0, e = Node->getNumOutputs(); i != e; ++i) {
    check_if_proceed(Visit(Node->getOutputExpr(i)));
  }

  for (unsigned i = 0, e = Node->getNumInputs(); i != e; ++i) {
    check_if_proceed(Visit(Node->getInputExpr(i)));
  }

  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCapturedStmt(CapturedStmt *Node) {
  return Visit(Node->getCapturedDecl()->getBody());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCAtTryStmt(ObjCAtTryStmt *Node) {
  if (auto *TS = dyn_cast<CompoundStmt>(Node->getTryBody())) {
    check_if_proceed(Visit(TS));
  }

  for (ObjCAtCatchStmt *catchStmt : Node->catch_stmts()) {
    if (auto *CS = dyn_cast<CompoundStmt>(catchStmt->getCatchBody())) {
      check_if_proceed(Visit(CS));
    }
  }

  if (auto *FS = static_cast<ObjCAtFinallyStmt *>(Node->getFinallyStmt())) {
    check_if_proceed(Visit(dyn_cast<CompoundStmt>(FS->getFinallyBody())));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCAtThrowStmt(ObjCAtThrowStmt *Node) {
  if (Node->getThrowExpr()) {
    return Visit(Node->getThrowExpr());
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCAtSynchronizedStmt(ObjCAtSynchronizedStmt *Node) {
  check_if_proceed(Visit(Node->getSynchExpr()));
  return Visit(Node->getSynchBody());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCAutoreleasePoolStmt(ObjCAutoreleasePoolStmt *Node) {
  return Visit(dyn_cast<CompoundStmt>(Node->getSubStmt()));
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitConstantExpr(ConstantExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCIvarRefExpr(ObjCIvarRefExpr *Node) {
  if (Node->getBase()) {
      return Visit(Node->getBase());
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCSubscriptRefExpr(ObjCSubscriptRefExpr *Node) {
  check_if_proceed(Visit(Node->getBaseExpr()));
  return Visit(Node->getKeyExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitImaginaryLiteral(ImaginaryLiteral *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitParenExpr(ParenExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitUnaryOperator(UnaryOperator *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOffsetOfExpr(OffsetOfExpr *Node) {
  for (unsigned i = 0, n = Node->getNumComponents(); i < n; ++i) {
    OffsetOfNode ON = Node->getComponent(i);
    if (ON.getKind() == OffsetOfNode::Array) {
      check_if_proceed(Visit(Node->getIndexExpr(ON.getArrayExprIndex())));
    }
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitUnaryExprOrTypeTraitExpr(
    UnaryExprOrTypeTraitExpr *Node) {
  if (!Node->isArgumentType()) {
    return Visit(Node->getArgumentExpr());
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitGenericSelectionExpr(GenericSelectionExpr *Node) {
  check_if_proceed(Visit(Node->getControllingExpr()));
  for (const GenericSelectionExpr::Association Assoc : Node->associations()) {
    check_if_proceed(Visit(Assoc.getAssociationExpr()));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitArraySubscriptExpr(ArraySubscriptExpr *Node) {
  check_if_proceed(Visit(Node->getLHS()));
  return Visit(Node->getRHS());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitMatrixSubscriptExpr(MatrixSubscriptExpr *Node) {
  check_if_proceed(Visit(Node->getBase()));
  check_if_proceed(Visit(Node->getRowIdx()));
  return Visit(Node->getColumnIdx());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPArraySectionExpr(OMPArraySectionExpr *Node) {
  check_if_proceed(Visit(Node->getBase()));
  if (Node->getLowerBound())
    check_if_proceed(Visit(Node->getLowerBound());)
  if (Node->getColonLocFirst().isValid()) {
    if (Node->getLength())
      check_if_proceed(Visit(Node->getLength()));
  }
  if (Node->getColonLocSecond().isValid()) {
    if (Node->getStride())
      check_if_proceed(Visit(Node->getStride()));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPArrayShapingExpr(OMPArrayShapingExpr *Node) {
  for (Expr *E : Node->getDimensions()) {
    check_if_proceed(Visit(E));
  }
  return Visit(Node->getBase());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPIteratorExpr(OMPIteratorExpr *Node) {
  for (unsigned I = 0, E = Node->numOfIterators(); I < E; ++I) {
    const OMPIteratorExpr::IteratorRange Range = Node->getIteratorRange(I);
    check_if_proceed(Visit(Range.Begin));
    check_if_proceed(Visit(Range.End));
    if (Range.Step) {
      check_if_proceed(Visit(Range.Step));
    }
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCallArgs(CallExpr *Call) {
  for (unsigned i = 0, e = Call->getNumArgs(); i != e; ++i) {
    check_if_proceed(Visit(Call->getArg(i)));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitMemberExpr(MemberExpr *Node) {
  return Visit(Node->getBase());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCIsaExpr(ObjCIsaExpr *Node) {
  return Visit(Node->getBase());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitExtVectorElementExpr(ExtVectorElementExpr *Node) {
  return Visit(Node->getBase());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCStyleCastExpr(CStyleCastExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCompoundLiteralExpr(CompoundLiteralExpr *Node) {
  return Visit(Node->getInitializer());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitImplicitCastExpr(ImplicitCastExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitBinaryOperator(BinaryOperator *Node) {
  check_if_proceed(Visit(Node->getLHS()));
  return Visit(Node->getRHS());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCompoundAssignOperator(CompoundAssignOperator *Node) {
  check_if_proceed(Visit(Node->getLHS()));
  return Visit(Node->getRHS());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitConditionalOperator(ConditionalOperator *Node) {
  check_if_proceed(Visit(Node->getCond()));
  check_if_proceed(Visit(Node->getLHS()));
  return Visit(Node->getRHS());
}

// GNU extensions.

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitBinaryConditionalOperator(BinaryConditionalOperator *Node) {
  check_if_proceed(Visit(Node->getCommon()));
  return Visit(Node->getFalseExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitStmtExpr(StmtExpr *E) {
  return Visit(E->getSubStmt());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitChooseExpr(ChooseExpr *Node) {
  check_if_proceed(Visit(Node->getCond()));
  check_if_proceed(Visit(Node->getLHS()));
  return Visit(Node->getRHS());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitShuffleVectorExpr(ShuffleVectorExpr *Node) {
  for (unsigned i = 0, e = Node->getNumSubExprs(); i != e; ++i) {
    check_if_proceed(Visit(Node->getExpr(i)));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitConvertVectorExpr(ConvertVectorExpr *Node) {
  return Visit(Node->getSrcExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitInitListExpr(InitListExpr* Node) {
  if (Node->getSyntacticForm()) {
    return Visit(Node->getSyntacticForm());
  }

  for (unsigned i = 0, e = Node->getNumInits(); i != e; ++i) {
    if (Node->getInit(i))
      check_if_proceed(Visit(Node->getInit(i)));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitArrayInitLoopExpr(ArrayInitLoopExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitParenListExpr(ParenListExpr* Node) {
  for (unsigned i = 0, e = Node->getNumExprs(); i != e; ++i) {
    check_if_proceed(Visit(Node->getExpr(i)));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitDesignatedInitExpr(DesignatedInitExpr *Node) {
  for (const DesignatedInitExpr::Designator &D : Node->designators()) {
    if (!D.isFieldDesignator()) {
      if (D.isArrayDesignator()) {
        check_if_proceed(Visit(Node->getArrayIndex(D)));
      } else {
        check_if_proceed(Visit(Node->getArrayRangeStart(D)));
        check_if_proceed(Visit(Node->getArrayRangeEnd(D)));
      }
    }
  }
  return Visit(Node->getInit());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitDesignatedInitUpdateExpr(
    DesignatedInitUpdateExpr *Node) {
  check_if_proceed(Visit(Node->getBase()));
  return Visit(Node->getUpdater());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitVAArgExpr(VAArgExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitPseudoObjectExpr(PseudoObjectExpr *Node) {
  return Visit(Node->getSyntacticForm());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitAtomicExpr(AtomicExpr *Node) {
  // AtomicExpr stores its subexpressions in a permuted order.
  check_if_proceed(Visit(Node->getPtr()));
  if (Node->getOp() != AtomicExpr::AO__c11_atomic_load &&
      Node->getOp() != AtomicExpr::AO__atomic_load_n &&
      Node->getOp() != AtomicExpr::AO__opencl_atomic_load &&
      Node->getOp() != AtomicExpr::AO__hip_atomic_load) {
    check_if_proceed(Visit(Node->getVal1()));
  }
  if (Node->getOp() == AtomicExpr::AO__atomic_exchange ||
      Node->isCmpXChg()) {
    check_if_proceed(Visit(Node->getVal2()));
  }
  if (Node->getOp() == AtomicExpr::AO__atomic_compare_exchange ||
      Node->getOp() == AtomicExpr::AO__atomic_compare_exchange_n) {
    check_if_proceed(Visit(Node->getWeak()));
  }
  if (Node->getOp() != AtomicExpr::AO__c11_atomic_init &&
      Node->getOp() != AtomicExpr::AO__opencl_atomic_init) {
    check_if_proceed(Visit(Node->getOrder()));
  }
  if (Node->isCmpXChg()) {
    check_if_proceed(Visit(Node->getOrderFail()));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

// C++
StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXOperatorCallExpr(CXXOperatorCallExpr *Node) {
  OverloadedOperatorKind Kind = Node->getOperator();
  if (Kind == OO_PlusPlus || Kind == OO_MinusMinus) {
      check_if_proceed(Visit(Node->getArg(0)));
  } 
  else if (Kind == OO_Arrow) {
    check_if_proceed(Visit(Node->getArg(0)));
  } 
  else if (Kind == OO_Call || Kind == OO_Subscript) {
    check_if_proceed(Visit(Node->getArg(0)));
    for (unsigned ArgIdx = 1; ArgIdx < Node->getNumArgs(); ++ArgIdx) {
        check_if_proceed(Visit(Node->getArg(ArgIdx)));
    }
  } 
  else if (Node->getNumArgs() == 1) {
    check_if_proceed(Visit(Node->getArg(0)));
  } 
  else if (Node->getNumArgs() == 2) {
    check_if_proceed(Visit(Node->getArg(0)));
    check_if_proceed(Visit(Node->getArg(1)));
  } 
  else {
    llvm_unreachable("unknown overloaded operator");
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXMemberCallExpr(CXXMemberCallExpr *Node) {
  CXXMethodDecl *MD = Node->getMethodDecl();
  if (MD && isa<CXXConversionDecl>(MD)) {
    return Visit(Node->getImplicitObjectArgument());
  }
  return VisitCallExpr(cast<CallExpr>(Node));
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCUDAKernelCallExpr(CUDAKernelCallExpr *Node) {
  check_if_proceed(Visit(Node->getCallee()));
  check_if_proceed(VisitCallArgs(Node->getConfig()));
  return VisitCallArgs(Node);
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXRewrittenBinaryOperator(
    CXXRewrittenBinaryOperator *Node) {
  CXXRewrittenBinaryOperator::DecomposedForm Decomposed =
      Node->getDecomposedForm();
  check_if_proceed(Visit(const_cast<Expr*>(Decomposed.LHS)));
  return Visit(const_cast<Expr*>(Decomposed.RHS));
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXStaticCastExpr(CXXStaticCastExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXDynamicCastExpr(CXXDynamicCastExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXReinterpretCastExpr(CXXReinterpretCastExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXConstCastExpr(CXXConstCastExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitBuiltinBitCastExpr(BuiltinBitCastExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXAddrspaceCastExpr(CXXAddrspaceCastExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXTypeidExpr(CXXTypeidExpr *Node) {
  if (!Node->isTypeOperand()) {
    return Visit(Node->getExprOperand());
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXUuidofExpr(CXXUuidofExpr *Node) {
  if (!Node->isTypeOperand()) {
    return Visit(Node->getExprOperand());
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitMSPropertyRefExpr(MSPropertyRefExpr *Node) {
  return Visit(Node->getBaseExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitMSPropertySubscriptExpr(MSPropertySubscriptExpr *Node) {
  check_if_proceed(Visit(Node->getBase()));
  return Visit(Node->getIdx());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitUserDefinedLiteral(UserDefinedLiteral *Node) {
  switch (Node->getLiteralOperatorKind()) {
    case UserDefinedLiteral::LOK_Character:
      return Visit(Node->getCookedLiteral());
      break;
    default:
      break;
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXThrowExpr(CXXThrowExpr *Node) {
  if (Node->getSubExpr())
    return Visit(Node->getSubExpr());
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXFunctionalCastExpr(CXXFunctionalCastExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *Node) {
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXTemporaryObjectExpr(CXXTemporaryObjectExpr *Node) {
  for (CXXTemporaryObjectExpr::arg_iterator Arg = Node->arg_begin(),
                                         ArgEnd = Node->arg_end();
       Arg != ArgEnd; ++Arg) {
    check_if_proceed(Visit(*Arg));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitLambdaExpr(LambdaExpr *Node) {
  for (LambdaExpr::capture_iterator C = Node->explicit_capture_begin(),
                                 CEnd = Node->explicit_capture_end();
       C != CEnd;
       ++C) {
    if (Node->isInitCapture(C)) {
      VarDecl *D = C->getCapturedVar();
      check_if_proceed(Visit(D->getInit()));
    }
  }

  return Visit(Node->getCompoundStmtBody());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXNewExpr(CXXNewExpr *E) {
  unsigned NumPlace = E->getNumPlacementArgs();
  if (NumPlace > 0 && !isa<CXXDefaultArgExpr>(E->getPlacementArg(0))) {
    check_if_proceed(Visit(E->getPlacementArg(0)));
    for (unsigned i = 1; i < NumPlace; ++i) {
      check_if_proceed(Visit(E->getPlacementArg(i)));
    }
  }

  CXXNewExpr::InitializationStyle InitStyle = E->getInitializationStyle();
  if (InitStyle != CXXNewExpr::NoInit) {
    return Visit(E->getInitializer());
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXDeleteExpr(CXXDeleteExpr *E) {
  return Visit(E->getArgument());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXPseudoDestructorExpr(CXXPseudoDestructorExpr *E) {
  return Visit(E->getBase());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXConstructExpr(CXXConstructExpr *E) {
  for (unsigned i = 0, e = E->getNumArgs(); i != e; ++i) {
    check_if_proceed(Visit(E->getArg(i)));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXStdInitializerListExpr(CXXStdInitializerListExpr *E) {
  return Visit(E->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitExprWithCleanups(ExprWithCleanups *E) {
  return Visit(E->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXUnresolvedConstructExpr(
    CXXUnresolvedConstructExpr *Node) {
  for (auto Arg = Node->arg_begin(), ArgEnd = Node->arg_end(); Arg != ArgEnd;
       ++Arg) {
    check_if_proceed(Visit(*Arg));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXDependentScopeMemberExpr(
                                         CXXDependentScopeMemberExpr *Node) {
  if (!Node->isImplicitAccess()) {
    return Visit(Node->getBase());
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitUnresolvedMemberExpr(UnresolvedMemberExpr *Node) {
  if (!Node->isImplicitAccess()) {
    return Visit(Node->getBase());
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitExpressionTraitExpr(ExpressionTraitExpr *E) {
  return Visit(E->getQueriedExpression());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXNoexceptExpr(CXXNoexceptExpr *E) {
  return Visit(E->getOperand());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitPackExpansionExpr(PackExpansionExpr *E) {
  return Visit(E->getPattern());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSubstNonTypeTemplateParmExpr(
                                       SubstNonTypeTemplateParmExpr *Node) {
  return Visit(Node->getReplacement());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitMaterializeTemporaryExpr(MaterializeTemporaryExpr *Node){
  return Visit(Node->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXFoldExpr(CXXFoldExpr *E) {
  if (E->getLHS()) {
    check_if_proceed(Visit(E->getLHS()));
  }
  if (E->getRHS()) {
    check_if_proceed(Visit(E->getRHS()));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitRequiresExpr(RequiresExpr *E) {
  auto Requirements = E->getRequirements();
  for (concepts::Requirement *Req : Requirements) {
    if (auto *ExprReq = dyn_cast<concepts::ExprRequirement>(Req)) {
      if (!ExprReq->isExprSubstitutionFailure())
        check_if_proceed(Visit(ExprReq->getExpr()));
    } 
    else {
      auto *NestedReq = cast<concepts::NestedRequirement>(Req);
      if (!NestedReq->isSubstitutionFailure())
        check_if_proceed(Visit(NestedReq->getConstraintExpr()));
    }
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

// C++ Coroutines TS

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCoroutineBodyStmt(CoroutineBodyStmt *S) {
  return Visit(S->getBody());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCoreturnStmt(CoreturnStmt *S) {
  if (S->getOperand()) {
    return Visit(S->getOperand());
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCoawaitExpr(CoawaitExpr *S) {
  return Visit(S->getOperand());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitDependentCoawaitExpr(DependentCoawaitExpr *S) {
  return Visit(S->getOperand());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCoyieldExpr(CoyieldExpr *S) {
  return Visit(S->getOperand());
}

// Obj-C

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCBoxedExpr(ObjCBoxedExpr *E) {
  return Visit(E->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCArrayLiteral(ObjCArrayLiteral *E) {
  ObjCArrayLiteral::child_range Ch = E->children();
  for (auto I = Ch.begin(), E = Ch.end(); I != E; ++I) {
    check_if_proceed(Visit(*I));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCDictionaryLiteral(ObjCDictionaryLiteral *E) {
  for (unsigned I = 0, N = E->getNumElements(); I != N; ++I) {
    ObjCDictionaryElement Element = E->getKeyValueElement(I);
    check_if_proceed(Visit(Element.Key));
    check_if_proceed(Visit(Element.Value));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCMessageExpr(ObjCMessageExpr *Mess) {
  switch (Mess->getReceiverKind()) {
  case ObjCMessageExpr::Instance:
    check_if_proceed(Visit(Mess->getInstanceReceiver()));
    break;
  default:
    break;
  }

  Selector selector = Mess->getSelector();
  if (!selector.isUnarySelector()) {
    for (unsigned i = 0, e = Mess->getNumArgs(); i != e; ++i) {
      check_if_proceed(Visit(Mess->getArg(i)));
    }
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCIndirectCopyRestoreExpr(ObjCIndirectCopyRestoreExpr *E) {
  return Visit(E->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCBridgedCastExpr(ObjCBridgedCastExpr *E) {
  return Visit(E->getSubExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOpaqueValueExpr(OpaqueValueExpr *Node) {
  return Visit(Node->getSourceExpr());
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitTypoExpr(TypoExpr *Node) {
  // TODO: Print something reasonable for a TypoExpr, if necessary.
  llvm_unreachable("Cannot print TypoExpr nodes");
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitRecoveryExpr(RecoveryExpr *Node) {
  for (Expr *E : Node->subExpressions()) {
    check_if_proceed(Visit(E));
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitAsTypeExpr(AsTypeExpr *Node) {
  return Visit(Node->getSrcExpr());
}

//--------------------------------

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitNullStmt(NullStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitDeclStmt(DeclStmt *Node) {
  for(Decl * d : Node->decls()) {
    declvisitor->Visit(d);
  }
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCForCollectionStmt(ObjCForCollectionStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXForRangeStmt(CXXForRangeStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitMSAsmStmt(MSAsmStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCAtFinallyStmt(ObjCAtFinallyStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCAtCatchStmt (ObjCAtCatchStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCAvailabilityCheckExpr(ObjCAvailabilityCheckExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXCatchStmt(CXXCatchStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXTryStmt(CXXTryStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSEHTryStmt(SEHTryStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSEHExceptStmt(SEHExceptStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSEHFinallyStmt(SEHFinallyStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSEHLeaveStmt(SEHLeaveStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitPragmaLiebherrStmt(PragmaLiebherrStmt *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

//===----------------------------------------------------------------------===//
//  OpenMP directives printing methods
//===----------------------------------------------------------------------===//

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPCanonicalLoop(OMPCanonicalLoop *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPMetaDirective(OMPMetaDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelDirective(OMPParallelDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPSimdDirective(OMPSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTileDirective(OMPTileDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPUnrollDirective(OMPUnrollDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPForDirective(OMPForDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPForSimdDirective(OMPForSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPSectionsDirective(OMPSectionsDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPSectionDirective(OMPSectionDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPSingleDirective(OMPSingleDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPMasterDirective(OMPMasterDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPCriticalDirective(OMPCriticalDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelForDirective(OMPParallelForDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelForSimdDirective(
    OMPParallelForSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelMasterDirective(
    OMPParallelMasterDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelMaskedDirective(
    OMPParallelMaskedDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelSectionsDirective(
    OMPParallelSectionsDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTaskDirective(OMPTaskDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTaskyieldDirective(OMPTaskyieldDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPBarrierDirective(OMPBarrierDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTaskwaitDirective(OMPTaskwaitDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTaskgroupDirective(OMPTaskgroupDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPFlushDirective(OMPFlushDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPDepobjDirective(OMPDepobjDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPScanDirective(OMPScanDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPOrderedDirective(OMPOrderedDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPAtomicDirective(OMPAtomicDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetDirective(OMPTargetDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetDataDirective(OMPTargetDataDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetEnterDataDirective(
    OMPTargetEnterDataDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetExitDataDirective(
    OMPTargetExitDataDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetParallelDirective(
    OMPTargetParallelDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetParallelForDirective(
    OMPTargetParallelForDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTeamsDirective(OMPTeamsDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPCancellationPointDirective(
    OMPCancellationPointDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPCancelDirective(OMPCancelDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTaskLoopDirective(OMPTaskLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTaskLoopSimdDirective(
    OMPTaskLoopSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPMasterTaskLoopDirective(
    OMPMasterTaskLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPMaskedTaskLoopDirective(
    OMPMaskedTaskLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPMasterTaskLoopSimdDirective(
    OMPMasterTaskLoopSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPMaskedTaskLoopSimdDirective(
    OMPMaskedTaskLoopSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelMasterTaskLoopDirective(
    OMPParallelMasterTaskLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelMaskedTaskLoopDirective(
    OMPParallelMaskedTaskLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelMasterTaskLoopSimdDirective(
    OMPParallelMasterTaskLoopSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelMaskedTaskLoopSimdDirective(
    OMPParallelMaskedTaskLoopSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPDistributeDirective(OMPDistributeDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetUpdateDirective(
    OMPTargetUpdateDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPDistributeParallelForDirective(
    OMPDistributeParallelForDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPDistributeParallelForSimdDirective(
    OMPDistributeParallelForSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPDistributeSimdDirective(
    OMPDistributeSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetParallelForSimdDirective(
    OMPTargetParallelForSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetSimdDirective(OMPTargetSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTeamsDistributeDirective(
    OMPTeamsDistributeDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTeamsDistributeSimdDirective(
    OMPTeamsDistributeSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTeamsDistributeParallelForSimdDirective(
    OMPTeamsDistributeParallelForSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTeamsDistributeParallelForDirective(
    OMPTeamsDistributeParallelForDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetTeamsDirective(OMPTargetTeamsDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetTeamsDistributeDirective(
    OMPTargetTeamsDistributeDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetTeamsDistributeParallelForDirective(
    OMPTargetTeamsDistributeParallelForDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetTeamsDistributeParallelForSimdDirective(
    OMPTargetTeamsDistributeParallelForSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetTeamsDistributeSimdDirective(
    OMPTargetTeamsDistributeSimdDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPInteropDirective(OMPInteropDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPDispatchDirective(OMPDispatchDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPMaskedDirective(OMPMaskedDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPGenericLoopDirective(OMPGenericLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTeamsGenericLoopDirective(
    OMPTeamsGenericLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetTeamsGenericLoopDirective(
    OMPTargetTeamsGenericLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPParallelGenericLoopDirective(
    OMPParallelGenericLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitOMPTargetParallelGenericLoopDirective(
    OMPTargetParallelGenericLoopDirective *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSourceLocExpr(SourceLocExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}


StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitDeclRefExpr(DeclRefExpr *Node) {
  declvisitor->Visit(Node->getDecl());
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitDependentScopeDeclRefExpr(
                                           DependentScopeDeclRefExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitUnresolvedLookupExpr(UnresolvedLookupExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCPropertyRefExpr(ObjCPropertyRefExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}


StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSYCLUniqueStableNameExpr(
    SYCLUniqueStableNameExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitPredefinedExpr(PredefinedExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitStringLiteral(StringLiteral *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCharacterLiteral(CharacterLiteral *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitIntegerLiteral(IntegerLiteral *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitFixedPointLiteral(FixedPointLiteral *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitFloatingLiteral(FloatingLiteral *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitAddrLabelExpr(AddrLabelExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitGNUNullExpr(GNUNullExpr *) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitArrayInitIndexExpr(ArrayInitIndexExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitNoInitExpr(NoInitExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitImplicitValueInitExpr(ImplicitValueInitExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXBoolLiteralExpr(CXXBoolLiteralExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXNullPtrLiteralExpr(CXXNullPtrLiteralExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXThisExpr(CXXThisExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}


StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXDefaultArgExpr(CXXDefaultArgExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXDefaultInitExpr(CXXDefaultInitExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXScalarValueInitExpr(CXXScalarValueInitExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitCXXInheritedCtorInitExpr(CXXInheritedCtorInitExpr *E) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitTypeTraitExpr(TypeTraitExpr *E) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitArrayTypeTraitExpr(ArrayTypeTraitExpr *E) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSizeOfPackExpr(SizeOfPackExpr *E) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitSubstNonTypeTemplateParmPackExpr(
                                       SubstNonTypeTemplateParmPackExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}


StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitFunctionParmPackExpr(FunctionParmPackExpr *E) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitConceptSpecializationExpr(ConceptSpecializationExpr *E) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCStringLiteral(ObjCStringLiteral *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCEncodeExpr(ObjCEncodeExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCSelectorExpr(ObjCSelectorExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCProtocolExpr(ObjCProtocolExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}


StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitObjCBoolLiteralExpr(ObjCBoolLiteralExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

StatInfASTExtendExecInfoStmt_ns::STATUS StatInfASTExtendExecInfoStmt::VisitBlockExpr(BlockExpr *Node) {
  return StatInfASTExtendExecInfoStmt_ns::STATUS::PROCEED;
}

}