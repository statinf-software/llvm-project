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

#include "clang/AST/StatInfInstrStmtPrinter.h"

namespace clang {

//===----------------------------------------------------------------------===//
//  Stmt printing methods.
//===----------------------------------------------------------------------===//

/// PrintRawCompoundStmt - Print a compound stmt without indenting the {, and
/// with no newline after the }.
void StatInfInstrStmtPrinter::PrintRawCompoundStmt(CompoundStmt *Node) {
  OS << "{" << NL;
  PrintFPPragmas(Node);
  
  bool loc_enter_function_body = enter_function_body;
  enter_function_body = false;
  if(EnableTemporalAnalysis && enable_instrumentation && loc_enter_function_body)
    Indent(Policy.Indentation) << "STATINF_ENTER_FUNCTION();" << NL;

  bool loc_enter_loop_body = enter_loop_body;
  enter_loop_body = false;
  if(EnableStructuralAnalysis && enable_instrumentation && loc_enter_loop_body)
    Indent(Policy.Indentation) << "STATINF_ENTER_LOOP();" << NL;

  bool loc_enter_then_body = enter_then_body;
  enter_then_body = false;
  if(EnableStructuralAnalysis && enable_instrumentation && loc_enter_then_body)
    Indent(Policy.Indentation) << "STATINF_ENTER_THEN();" << NL;

  bool loc_enter_else_body = enter_else_body;
  enter_else_body = false;
  if(EnableStructuralAnalysis && enable_instrumentation && loc_enter_else_body)
    Indent(Policy.Indentation) << "STATINF_ENTER_ELSE();" << NL;

  for (auto *I : Node->body()) {
    if(isa<DeclStmt>(I))
      PrintStmt(I);
  }

  bool loc_enter_main_function = enter_main_function;
  enter_main_function = false;
  if((EnableStructuralAnalysis || EnableTemporalAnalysis) && loc_enter_main_function) {
    Indent(Policy.Indentation) << "STATINF_MAIN_INIT();" << NL;
  }

  for (auto *I : Node->body()) {
    if(!isa<DeclStmt>(I))
      PrintStmt(I);
  }

  if(EnableTemporalAnalysis && enable_instrumentation && loc_enter_function_body) {
    if(!isa<ReturnStmt>(Node->body_back()))
      Indent(Policy.Indentation) << "STATINF_EXIT_FUNCTION();" << NL; 
  }

  Indent() << "}";
}

void StatInfInstrStmtPrinter::PrintFPPragmas(CompoundStmt *S) {
  if (!S->hasStoredFPFeatures())
    return;
  FPOptionsOverride FPO = S->getStoredFPFeatures();
  bool FEnvAccess = false;
  if (FPO.hasAllowFEnvAccessOverride()) {
    FEnvAccess = FPO.getAllowFEnvAccessOverride();
    Indent() << "#pragma STDC FENV_ACCESS " << (FEnvAccess ? "ON" : "OFF")
             << NL;
  }
  if (FPO.hasSpecifiedExceptionModeOverride()) {
    LangOptions::FPExceptionModeKind EM =
        FPO.getSpecifiedExceptionModeOverride();
    if (!FEnvAccess || EM != LangOptions::FPE_Strict) {
      Indent() << "#pragma clang fp exceptions(";
      switch (FPO.getSpecifiedExceptionModeOverride()) {
      default:
        break;
      case LangOptions::FPE_Ignore:
        OS << "ignore";
        break;
      case LangOptions::FPE_MayTrap:
        OS << "maytrap";
        break;
      case LangOptions::FPE_Strict:
        OS << "strict";
        break;
      }
      OS << ")\n";
    }
  }
  if (FPO.hasConstRoundingModeOverride()) {
    LangOptions::RoundingMode RM = FPO.getConstRoundingModeOverride();
    Indent() << "#pragma STDC FENV_ROUND ";
    switch (RM) {
    case llvm::RoundingMode::TowardZero:
      OS << "FE_TOWARDZERO";
      break;
    case llvm::RoundingMode::NearestTiesToEven:
      OS << "FE_TONEAREST";
      break;
    case llvm::RoundingMode::TowardPositive:
      OS << "FE_UPWARD";
      break;
    case llvm::RoundingMode::TowardNegative:
      OS << "FE_DOWNWARD";
      break;
    case llvm::RoundingMode::NearestTiesToAway:
      OS << "FE_TONEARESTFROMZERO";
      break;
    case llvm::RoundingMode::Dynamic:
      OS << "FE_DYNAMIC";
      break;
    default:
      llvm_unreachable("Invalid rounding mode");
    }
    OS << NL;
  }
}

void StatInfInstrStmtPrinter::PrintRawDecl(Decl *D) {
  D->print(OS, Policy, IndentLevel);
}

void StatInfInstrStmtPrinter::PrintRawDeclStmt(const DeclStmt *S) {
  SmallVector<Decl *, 2> Decls(S->decls());
  Decl::printGroup(Decls.data(), Decls.size(), OS, Policy, IndentLevel);
}

void StatInfInstrStmtPrinter::VisitNullStmt(NullStmt *Node) {
  Indent() << ";" << NL;
}

void StatInfInstrStmtPrinter::VisitDeclStmt(DeclStmt *Node) {
  Indent();
  PrintRawDeclStmt(Node);
  OS << ";" << NL;
}

void StatInfInstrStmtPrinter::VisitCompoundStmt(CompoundStmt *Node) {
  Indent();
  PrintRawCompoundStmt(Node);
  OS << "" << NL;
}

void StatInfInstrStmtPrinter::VisitCaseStmt(CaseStmt *Node) {
  Indent(-1) << "case ";
  PrintExpr(Node->getLHS());
  if (Node->getRHS()) {
    OS << " ... ";
    PrintExpr(Node->getRHS());
  }
  OS << ":" << NL;

  if(EnableStructuralAnalysis && enable_instrumentation)
    Indent(Policy.Indentation) << "STATINF_SWITCH_CASE("<< switch_case_count++ << ")" << NL;

  PrintStmt(Node->getSubStmt(), 0);
}

void StatInfInstrStmtPrinter::VisitDefaultStmt(DefaultStmt *Node) {
  Indent(-1) << "default:" << NL;
  if(EnableStructuralAnalysis && enable_instrumentation)
    Indent(Policy.Indentation) << "STATINF_SWITCH_CASE("<< switch_case_count++ << ");" << NL;
  PrintStmt(Node->getSubStmt(), 0);
}

void StatInfInstrStmtPrinter::VisitLabelStmt(LabelStmt *Node) {
  Indent(-1) << Node->getName() << ":" << NL;
  PrintStmt(Node->getSubStmt(), 0);
}

void StatInfInstrStmtPrinter::VisitAttributedStmt(AttributedStmt *Node) {
  for (const auto *Attr : Node->getAttrs()) {
    Attr->printPretty(OS, Policy);
  }

  PrintStmt(Node->getSubStmt(), 0);
}

void StatInfInstrStmtPrinter::PrintRawIfStmt(IfStmt *If) {
  if (If->isConsteval()) {
    OS << "if ";
    if (If->isNegatedConsteval())
      OS << "!";
    OS << "consteval";
    OS << NL;

    if(isa<CompoundStmt>(If->getThen())) {
      SetEnterThenBody();
      PrintStmt(If->getThen());
    }
    else {
      OS << " {" << NL;
      if(EnableStructuralAnalysis && enable_instrumentation)
        Indent(Policy.Indentation) << "STATINF_ENTER_THEN();" << NL;
      PrintStmt(If->getThen());
      Indent() << "}" << NL;
    }

    Indent();
    OS << "else";
    Stmt *Else = nullptr;
    if ((Else = If->getElse()) && isa<CompoundStmt>(Else)) {
        SetEnterElseBody();
        PrintStmt(Else);
    }
    else {
      Indent() << " {" << NL;
      if(EnableStructuralAnalysis && enable_instrumentation)
        Indent(Policy.Indentation) << "STATINF_ENTER_ELSE();" << NL;
      if(Else)
        PrintStmt(Else);
      Indent() << "}";
    }
    OS << NL;

    return;
  }

  OS << "if (";
  if (If->getInit())
    PrintInitStmt(If->getInit(), 4);
  if (const DeclStmt *DS = If->getConditionVariableDeclStmt())
    PrintRawDeclStmt(DS);
  else
    PrintExpr(If->getCond());
  OS << ')';

  if (auto *CS = dyn_cast<CompoundStmt>(If->getThen())) {
    OS << ' ';
    PrintStmt(If->getThen());
    PrintRawCompoundStmt(CS);
    OS << (If->getElse() ? " " : NL);
  } else {
    OS << " {" << NL;
    if(EnableStructuralAnalysis && enable_instrumentation)
      Indent(Policy.Indentation) << "STATINF_ENTER_THEN();" << NL;
    PrintStmt(If->getThen());
    Indent() << "}" << NL;
  }
  Indent();
  OS << "else";
  Stmt *Else = nullptr;
  if (Else = If->getElse()) {
    if (auto *CS = dyn_cast<CompoundStmt>(Else)) {
      OS << ' ';
      SetEnterElseBody();
      PrintRawCompoundStmt(CS);
      OS << NL;
    } else if (auto *ElseIf = dyn_cast<IfStmt>(Else)) {
      OS << " {" << NL;
      if(EnableStructuralAnalysis && enable_instrumentation)
        Indent(Policy.Indentation) << "STATINF_ENTER_ELSE();" << NL;
      PrintRawIfStmt(ElseIf);
      Indent() << "}" << NL;
    } else {
      OS << " {" << NL;
      if(EnableStructuralAnalysis && enable_instrumentation)
        Indent(Policy.Indentation) << "STATINF_ENTER_ELSE();" << NL;
      PrintStmt(Else);
      Indent() << "}" << NL;
    }
  }
  else if(EnableStructuralAnalysis && enable_instrumentation) {
    OS << " {" << NL;
    Indent(Policy.Indentation) << "STATINF_ENTER_ELSE();" << NL;
    Indent() << "}" << NL;
  }
}

void StatInfInstrStmtPrinter::VisitIfStmt(IfStmt *If) {
  Indent();
  PrintRawIfStmt(If);
}

void StatInfInstrStmtPrinter::VisitSwitchStmt(SwitchStmt *Node) {
  if(EnableStructuralAnalysis && enable_instrumentation) {
    uint32_t num_cases = 0;
    CompoundStmt *body = dyn_cast<CompoundStmt>(Node->getBody());
    if(body) {
      for(auto ch : body->body()) {
        if(isa<CaseStmt>(ch) || isa<DefaultStmt>(ch))
          ++num_cases;
      }
    }
    else
      llvm::errs() << "Weird: Child of a Switch is not a CompoundStmt but a " << Node->getBody()->getStmtClassName();

    Indent() << "STATINF_INIT_SWITCH(" << num_cases << ");" << NL;
  }

  Indent() << "switch (";
  if (Node->getInit())
    PrintInitStmt(Node->getInit(), 8);
  if (const DeclStmt *DS = Node->getConditionVariableDeclStmt())
    PrintRawDeclStmt(DS);
  else
    PrintExpr(Node->getCond());
  OS << ")";
  switch_case_count = 0;
  PrintControlledStmt(Node->getBody());
}

void StatInfInstrStmtPrinter::VisitWhileStmt(WhileStmt *Node) {
  Indent() << "while (";
  if (const DeclStmt *DS = Node->getConditionVariableDeclStmt())
    PrintRawDeclStmt(DS);
  else
    PrintExpr(Node->getCond());
  OS << ")" << NL;
  if(isa<CompoundStmt>(Node->getBody())) {
    SetEnterLoopBody();
    PrintStmt(Node->getBody());
  }
  else {
    OS << " {" << NL;
    if(EnableStructuralAnalysis && enable_instrumentation)
      Indent(Policy.Indentation) << "STATINF_ENTER_LOOP();" << NL;
    PrintStmt(Node->getBody());
    OS << "}" << NL;
  }
  if(EnableStructuralAnalysis && enable_instrumentation)
    Indent() << "STATINF_EXIT_LOOP();" << NL;
}

void StatInfInstrStmtPrinter::VisitDoStmt(DoStmt *Node) {
  Indent() << "do ";
  if (auto *CS = dyn_cast<CompoundStmt>(Node->getBody())) {
    PrintRawCompoundStmt(CS);
    OS << " ";
  } else {
    OS << NL;
    PrintStmt(Node->getBody());
    Indent();
  }

  OS << "while (";
  PrintExpr(Node->getCond());
  OS << ");" << NL;
}

void StatInfInstrStmtPrinter::VisitForStmt(ForStmt *Node) {
  Indent() << "for (";
  if (Node->getInit())
    PrintInitStmt(Node->getInit(), 5);
  else
    OS << (Node->getCond() ? "; " : ";");
  if (Node->getCond())
    PrintExpr(Node->getCond());
  OS << ";";
  if (Node->getInc()) {
    OS << " ";
    PrintExpr(Node->getInc());
  }
  OS << ")";
  if(isa<CompoundStmt>(Node->getBody())) {
    SetEnterLoopBody();
    PrintControlledStmt(Node->getBody());
  }
  else {
    Indent() << " {" << NL;
    if(EnableStructuralAnalysis && enable_instrumentation)
      Indent(Policy.Indentation) << "STATINF_ENTER_LOOP();" << NL;
    PrintControlledStmt(Node->getBody());
    Indent() << "}" << NL;
  }
  if(EnableStructuralAnalysis && enable_instrumentation)
    Indent() << "STATINF_EXIT_LOOP();" << NL;
}

void StatInfInstrStmtPrinter::VisitObjCForCollectionStmt(ObjCForCollectionStmt *Node) {
  Indent() << "for (";
  if (auto *DS = dyn_cast<DeclStmt>(Node->getElement()))
    PrintRawDeclStmt(DS);
  else
    PrintExpr(cast<Expr>(Node->getElement()));
  OS << " in ";
  PrintExpr(Node->getCollection());
  OS << ")";
  PrintControlledStmt(Node->getBody());
}

void StatInfInstrStmtPrinter::VisitCXXForRangeStmt(CXXForRangeStmt *Node) {
  Indent() << "for (";
  if (Node->getInit())
    PrintInitStmt(Node->getInit(), 5);
  PrintingPolicy SubPolicy(Policy);
  SubPolicy.SuppressInitializers = true;
  Node->getLoopVariable()->print(OS, SubPolicy, IndentLevel);
  OS << " : ";
  PrintExpr(Node->getRangeInit());
  OS << ")";
  PrintControlledStmt(Node->getBody());
}

void StatInfInstrStmtPrinter::VisitMSDependentExistsStmt(MSDependentExistsStmt *Node) {
  Indent();
  if (Node->isIfExists())
    OS << "__if_exists (";
  else
    OS << "__if_not_exists (";

  if (NestedNameSpecifier *Qualifier
        = Node->getQualifierLoc().getNestedNameSpecifier())
    Qualifier->print(OS, Policy);

  OS << Node->getNameInfo() << ") ";

  PrintRawCompoundStmt(Node->getSubStmt());
}

void StatInfInstrStmtPrinter::VisitGotoStmt(GotoStmt *Node) {
  Indent() << "goto " << Node->getLabel()->getName() << ";";
  if (Policy.IncludeNewlines) OS << NL;
}

void StatInfInstrStmtPrinter::VisitIndirectGotoStmt(IndirectGotoStmt *Node) {
  Indent() << "goto *";
  PrintExpr(Node->getTarget());
  OS << ";";
  if (Policy.IncludeNewlines) OS << NL;
}

void StatInfInstrStmtPrinter::VisitContinueStmt(ContinueStmt *Node) {
  Indent() << "continue;";
  if (Policy.IncludeNewlines) OS << NL;
}

void StatInfInstrStmtPrinter::VisitBreakStmt(BreakStmt *Node) {
  Indent() << "break;";
  if (Policy.IncludeNewlines) OS << NL;
}

void StatInfInstrStmtPrinter::VisitReturnStmt(ReturnStmt *Node) {
  if(EnableTemporalAnalysis && enable_instrumentation)
    Indent() << "STATINF_EXIT_FUNCTION();" << NL; 
  Indent() << "return";
  if (Node->getRetValue()) {
    OS << " ";
    PrintExpr(Node->getRetValue());
  }
  OS << ";";
  if (Policy.IncludeNewlines) OS << NL;
}

void StatInfInstrStmtPrinter::VisitGCCAsmStmt(GCCAsmStmt *Node) {
  Indent() << "asm ";

  if (Node->isVolatile())
    OS << "volatile ";

  if (Node->isAsmGoto())
    OS << "goto ";

  OS << "(";
  VisitStringLiteral(Node->getAsmString());

  // Outputs
  if (Node->getNumOutputs() != 0 || Node->getNumInputs() != 0 ||
      Node->getNumClobbers() != 0 || Node->getNumLabels() != 0)
    OS << " : ";

  for (unsigned i = 0, e = Node->getNumOutputs(); i != e; ++i) {
    if (i != 0)
      OS << ", ";

    if (!Node->getOutputName(i).empty()) {
      OS << '[';
      OS << Node->getOutputName(i);
      OS << "] ";
    }

    VisitStringLiteral(Node->getOutputConstraintLiteral(i));
    OS << " (";
    Visit(Node->getOutputExpr(i));
    OS << ")";
  }

  // Inputs
  if (Node->getNumInputs() != 0 || Node->getNumClobbers() != 0 ||
      Node->getNumLabels() != 0)
    OS << " : ";

  for (unsigned i = 0, e = Node->getNumInputs(); i != e; ++i) {
    if (i != 0)
      OS << ", ";

    if (!Node->getInputName(i).empty()) {
      OS << '[';
      OS << Node->getInputName(i);
      OS << "] ";
    }

    VisitStringLiteral(Node->getInputConstraintLiteral(i));
    OS << " (";
    Visit(Node->getInputExpr(i));
    OS << ")";
  }

  // Clobbers
  if (Node->getNumClobbers() != 0 || Node->getNumLabels())
    OS << " : ";

  for (unsigned i = 0, e = Node->getNumClobbers(); i != e; ++i) {
    if (i != 0)
      OS << ", ";

    VisitStringLiteral(Node->getClobberStringLiteral(i));
  }

  // Labels
  if (Node->getNumLabels() != 0)
    OS << " : ";

  for (unsigned i = 0, e = Node->getNumLabels(); i != e; ++i) {
    if (i != 0)
      OS << ", ";
    OS << Node->getLabelName(i);
  }

  OS << ");";
  if (Policy.IncludeNewlines) OS << NL;
}

void StatInfInstrStmtPrinter::VisitMSAsmStmt(MSAsmStmt *Node) {
  // FIXME: Implement MS style inline asm statement printer.
  Indent() << "__asm ";
  if (Node->hasBraces())
    OS << "{" << NL;
  OS << Node->getAsmString() << NL;
  if (Node->hasBraces())
    Indent() << "}" << NL;
}

void StatInfInstrStmtPrinter::VisitCapturedStmt(CapturedStmt *Node) {
  PrintStmt(Node->getCapturedDecl()->getBody());
}

void StatInfInstrStmtPrinter::VisitObjCAtTryStmt(ObjCAtTryStmt *Node) {
  Indent() << "@try";
  if (auto *TS = dyn_cast<CompoundStmt>(Node->getTryBody())) {
    PrintRawCompoundStmt(TS);
    OS << NL;
  }

  for (ObjCAtCatchStmt *catchStmt : Node->catch_stmts()) {
    Indent() << "@catch(";
    if (Decl *DS = catchStmt->getCatchParamDecl())
      PrintRawDecl(DS);
    OS << ")";
    if (auto *CS = dyn_cast<CompoundStmt>(catchStmt->getCatchBody())) {
      PrintRawCompoundStmt(CS);
      OS << NL;
    }
  }

  if (auto *FS = static_cast<ObjCAtFinallyStmt *>(Node->getFinallyStmt())) {
    Indent() << "@finally";
    PrintRawCompoundStmt(dyn_cast<CompoundStmt>(FS->getFinallyBody()));
    OS << NL;
  }
}

void StatInfInstrStmtPrinter::VisitObjCAtFinallyStmt(ObjCAtFinallyStmt *Node) {
}

void StatInfInstrStmtPrinter::VisitObjCAtCatchStmt (ObjCAtCatchStmt *Node) {
  Indent() << "@catch (...) { /* todo */ } " << NL;
}

void StatInfInstrStmtPrinter::VisitObjCAtThrowStmt(ObjCAtThrowStmt *Node) {
  Indent() << "@throw";
  if (Node->getThrowExpr()) {
    OS << " ";
    PrintExpr(Node->getThrowExpr());
  }
  OS << ";" << NL;
}

void StatInfInstrStmtPrinter::VisitObjCAvailabilityCheckExpr(
    ObjCAvailabilityCheckExpr *Node) {
  OS << "@available(...)";
}

void StatInfInstrStmtPrinter::VisitObjCAtSynchronizedStmt(ObjCAtSynchronizedStmt *Node) {
  Indent() << "@synchronized (";
  PrintExpr(Node->getSynchExpr());
  OS << ")";
  PrintRawCompoundStmt(Node->getSynchBody());
  OS << NL;
}

void StatInfInstrStmtPrinter::VisitObjCAutoreleasePoolStmt(ObjCAutoreleasePoolStmt *Node) {
  Indent() << "@autoreleasepool";
  PrintRawCompoundStmt(dyn_cast<CompoundStmt>(Node->getSubStmt()));
  OS << NL;
}

void StatInfInstrStmtPrinter::PrintRawCXXCatchStmt(CXXCatchStmt *Node) {
  OS << "catch (";
  if (Decl *ExDecl = Node->getExceptionDecl())
    PrintRawDecl(ExDecl);
  else
    OS << "...";
  OS << ") ";
  PrintRawCompoundStmt(cast<CompoundStmt>(Node->getHandlerBlock()));
}

void StatInfInstrStmtPrinter::VisitCXXCatchStmt(CXXCatchStmt *Node) {
  Indent();
  PrintRawCXXCatchStmt(Node);
  OS << NL;
}

void StatInfInstrStmtPrinter::VisitCXXTryStmt(CXXTryStmt *Node) {
  Indent() << "try ";
  PrintRawCompoundStmt(Node->getTryBlock());
  for (unsigned i = 0, e = Node->getNumHandlers(); i < e; ++i) {
    OS << " ";
    PrintRawCXXCatchStmt(Node->getHandler(i));
  }
  OS << NL;
}

void StatInfInstrStmtPrinter::VisitSEHTryStmt(SEHTryStmt *Node) {
  Indent() << (Node->getIsCXXTry() ? "try " : "__try ");
  PrintRawCompoundStmt(Node->getTryBlock());
  SEHExceptStmt *E = Node->getExceptHandler();
  SEHFinallyStmt *F = Node->getFinallyHandler();
  if(E)
    PrintRawSEHExceptHandler(E);
  else {
    assert(F && "Must have a finally block...");
    PrintRawSEHFinallyStmt(F);
  }
  OS << NL;
}

void StatInfInstrStmtPrinter::PrintRawSEHFinallyStmt(SEHFinallyStmt *Node) {
  OS << "__finally ";
  PrintRawCompoundStmt(Node->getBlock());
  OS << NL;
}

void StatInfInstrStmtPrinter::PrintRawSEHExceptHandler(SEHExceptStmt *Node) {
  OS << "__except (";
  VisitExpr(Node->getFilterExpr());
  OS << ")" << NL;
  PrintRawCompoundStmt(Node->getBlock());
  OS << NL;
}

void StatInfInstrStmtPrinter::VisitSEHExceptStmt(SEHExceptStmt *Node) {
  Indent();
  PrintRawSEHExceptHandler(Node);
  OS << NL;
}

void StatInfInstrStmtPrinter::VisitSEHFinallyStmt(SEHFinallyStmt *Node) {
  Indent();
  PrintRawSEHFinallyStmt(Node);
  OS << NL;
}

void StatInfInstrStmtPrinter::VisitSEHLeaveStmt(SEHLeaveStmt *Node) {
  Indent() << "__leave;";
  if (Policy.IncludeNewlines) OS << NL;
}

//===----------------------------------------------------------------------===//
//  OpenMP directives printing methods
//===----------------------------------------------------------------------===//

void StatInfInstrStmtPrinter::VisitOMPCanonicalLoop(OMPCanonicalLoop *Node) {
  PrintStmt(Node->getLoopStmt());
}

void StatInfInstrStmtPrinter::PrintOMPExecutableDirective(OMPExecutableDirective *S,
                                              bool ForceNoStmt) {
  OMPClausePrinter Printer(OS, Policy);
  ArrayRef<OMPClause *> Clauses = S->clauses();
  for (auto *Clause : Clauses)
    if (Clause && !Clause->isImplicit()) {
      OS << ' ';
      Printer.Visit(Clause);
    }
  OS << NL;
  if (!ForceNoStmt && S->hasAssociatedStmt())
    PrintStmt(S->getRawStmt());
}

void StatInfInstrStmtPrinter::VisitOMPMetaDirective(OMPMetaDirective *Node) {
  Indent() << "#pragma omp metadirective";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelDirective(OMPParallelDirective *Node) {
  Indent() << "#pragma omp parallel";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPSimdDirective(OMPSimdDirective *Node) {
  Indent() << "#pragma omp simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTileDirective(OMPTileDirective *Node) {
  Indent() << "#pragma omp tile";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPUnrollDirective(OMPUnrollDirective *Node) {
  Indent() << "#pragma omp unroll";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPForDirective(OMPForDirective *Node) {
  Indent() << "#pragma omp for";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPForSimdDirective(OMPForSimdDirective *Node) {
  Indent() << "#pragma omp for simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPSectionsDirective(OMPSectionsDirective *Node) {
  Indent() << "#pragma omp sections";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPSectionDirective(OMPSectionDirective *Node) {
  Indent() << "#pragma omp section";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPSingleDirective(OMPSingleDirective *Node) {
  Indent() << "#pragma omp single";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPMasterDirective(OMPMasterDirective *Node) {
  Indent() << "#pragma omp master";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPCriticalDirective(OMPCriticalDirective *Node) {
  Indent() << "#pragma omp critical";
  if (Node->getDirectiveName().getName()) {
    OS << " (";
    Node->getDirectiveName().printName(OS, Policy);
    OS << ")";
  }
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelForDirective(OMPParallelForDirective *Node) {
  Indent() << "#pragma omp parallel for";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelForSimdDirective(
    OMPParallelForSimdDirective *Node) {
  Indent() << "#pragma omp parallel for simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelMasterDirective(
    OMPParallelMasterDirective *Node) {
  Indent() << "#pragma omp parallel master";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelMaskedDirective(
    OMPParallelMaskedDirective *Node) {
  Indent() << "#pragma omp parallel masked";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelSectionsDirective(
    OMPParallelSectionsDirective *Node) {
  Indent() << "#pragma omp parallel sections";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTaskDirective(OMPTaskDirective *Node) {
  Indent() << "#pragma omp task";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTaskyieldDirective(OMPTaskyieldDirective *Node) {
  Indent() << "#pragma omp taskyield";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPBarrierDirective(OMPBarrierDirective *Node) {
  Indent() << "#pragma omp barrier";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTaskwaitDirective(OMPTaskwaitDirective *Node) {
  Indent() << "#pragma omp taskwait";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPErrorDirective(OMPErrorDirective *Node) {
  Indent() << "#pragma omp error";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTaskgroupDirective(OMPTaskgroupDirective *Node) {
  Indent() << "#pragma omp taskgroup";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPFlushDirective(OMPFlushDirective *Node) {
  Indent() << "#pragma omp flush";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPDepobjDirective(OMPDepobjDirective *Node) {
  Indent() << "#pragma omp depobj";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPScanDirective(OMPScanDirective *Node) {
  Indent() << "#pragma omp scan";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPOrderedDirective(OMPOrderedDirective *Node) {
  Indent() << "#pragma omp ordered";
  PrintOMPExecutableDirective(Node, Node->hasClausesOfKind<OMPDependClause>());
}

void StatInfInstrStmtPrinter::VisitOMPAtomicDirective(OMPAtomicDirective *Node) {
  Indent() << "#pragma omp atomic";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetDirective(OMPTargetDirective *Node) {
  Indent() << "#pragma omp target";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetDataDirective(OMPTargetDataDirective *Node) {
  Indent() << "#pragma omp target data";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetEnterDataDirective(
    OMPTargetEnterDataDirective *Node) {
  Indent() << "#pragma omp target enter data";
  PrintOMPExecutableDirective(Node, /*ForceNoStmt=*/true);
}

void StatInfInstrStmtPrinter::VisitOMPTargetExitDataDirective(
    OMPTargetExitDataDirective *Node) {
  Indent() << "#pragma omp target exit data";
  PrintOMPExecutableDirective(Node, /*ForceNoStmt=*/true);
}

void StatInfInstrStmtPrinter::VisitOMPTargetParallelDirective(
    OMPTargetParallelDirective *Node) {
  Indent() << "#pragma omp target parallel";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetParallelForDirective(
    OMPTargetParallelForDirective *Node) {
  Indent() << "#pragma omp target parallel for";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTeamsDirective(OMPTeamsDirective *Node) {
  Indent() << "#pragma omp teams";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPCancellationPointDirective(
    OMPCancellationPointDirective *Node) {
  Indent() << "#pragma omp cancellation point "
           << getOpenMPDirectiveName(Node->getCancelRegion());
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPCancelDirective(OMPCancelDirective *Node) {
  Indent() << "#pragma omp cancel "
           << getOpenMPDirectiveName(Node->getCancelRegion());
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTaskLoopDirective(OMPTaskLoopDirective *Node) {
  Indent() << "#pragma omp taskloop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTaskLoopSimdDirective(
    OMPTaskLoopSimdDirective *Node) {
  Indent() << "#pragma omp taskloop simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPMasterTaskLoopDirective(
    OMPMasterTaskLoopDirective *Node) {
  Indent() << "#pragma omp master taskloop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPMaskedTaskLoopDirective(
    OMPMaskedTaskLoopDirective *Node) {
  Indent() << "#pragma omp masked taskloop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPMasterTaskLoopSimdDirective(
    OMPMasterTaskLoopSimdDirective *Node) {
  Indent() << "#pragma omp master taskloop simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPMaskedTaskLoopSimdDirective(
    OMPMaskedTaskLoopSimdDirective *Node) {
  Indent() << "#pragma omp masked taskloop simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelMasterTaskLoopDirective(
    OMPParallelMasterTaskLoopDirective *Node) {
  Indent() << "#pragma omp parallel master taskloop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelMaskedTaskLoopDirective(
    OMPParallelMaskedTaskLoopDirective *Node) {
  Indent() << "#pragma omp parallel masked taskloop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelMasterTaskLoopSimdDirective(
    OMPParallelMasterTaskLoopSimdDirective *Node) {
  Indent() << "#pragma omp parallel master taskloop simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelMaskedTaskLoopSimdDirective(
    OMPParallelMaskedTaskLoopSimdDirective *Node) {
  Indent() << "#pragma omp parallel masked taskloop simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPDistributeDirective(OMPDistributeDirective *Node) {
  Indent() << "#pragma omp distribute";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetUpdateDirective(
    OMPTargetUpdateDirective *Node) {
  Indent() << "#pragma omp target update";
  PrintOMPExecutableDirective(Node, /*ForceNoStmt=*/true);
}

void StatInfInstrStmtPrinter::VisitOMPDistributeParallelForDirective(
    OMPDistributeParallelForDirective *Node) {
  Indent() << "#pragma omp distribute parallel for";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPDistributeParallelForSimdDirective(
    OMPDistributeParallelForSimdDirective *Node) {
  Indent() << "#pragma omp distribute parallel for simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPDistributeSimdDirective(
    OMPDistributeSimdDirective *Node) {
  Indent() << "#pragma omp distribute simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetParallelForSimdDirective(
    OMPTargetParallelForSimdDirective *Node) {
  Indent() << "#pragma omp target parallel for simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetSimdDirective(OMPTargetSimdDirective *Node) {
  Indent() << "#pragma omp target simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTeamsDistributeDirective(
    OMPTeamsDistributeDirective *Node) {
  Indent() << "#pragma omp teams distribute";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTeamsDistributeSimdDirective(
    OMPTeamsDistributeSimdDirective *Node) {
  Indent() << "#pragma omp teams distribute simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTeamsDistributeParallelForSimdDirective(
    OMPTeamsDistributeParallelForSimdDirective *Node) {
  Indent() << "#pragma omp teams distribute parallel for simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTeamsDistributeParallelForDirective(
    OMPTeamsDistributeParallelForDirective *Node) {
  Indent() << "#pragma omp teams distribute parallel for";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetTeamsDirective(OMPTargetTeamsDirective *Node) {
  Indent() << "#pragma omp target teams";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetTeamsDistributeDirective(
    OMPTargetTeamsDistributeDirective *Node) {
  Indent() << "#pragma omp target teams distribute";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetTeamsDistributeParallelForDirective(
    OMPTargetTeamsDistributeParallelForDirective *Node) {
  Indent() << "#pragma omp target teams distribute parallel for";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetTeamsDistributeParallelForSimdDirective(
    OMPTargetTeamsDistributeParallelForSimdDirective *Node) {
  Indent() << "#pragma omp target teams distribute parallel for simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetTeamsDistributeSimdDirective(
    OMPTargetTeamsDistributeSimdDirective *Node) {
  Indent() << "#pragma omp target teams distribute simd";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPInteropDirective(OMPInteropDirective *Node) {
  Indent() << "#pragma omp interop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPDispatchDirective(OMPDispatchDirective *Node) {
  Indent() << "#pragma omp dispatch";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPMaskedDirective(OMPMaskedDirective *Node) {
  Indent() << "#pragma omp masked";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPGenericLoopDirective(OMPGenericLoopDirective *Node) {
  Indent() << "#pragma omp loop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTeamsGenericLoopDirective(
    OMPTeamsGenericLoopDirective *Node) {
  Indent() << "#pragma omp teams loop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetTeamsGenericLoopDirective(
    OMPTargetTeamsGenericLoopDirective *Node) {
  Indent() << "#pragma omp target teams loop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPParallelGenericLoopDirective(
    OMPParallelGenericLoopDirective *Node) {
  Indent() << "#pragma omp parallel loop";
  PrintOMPExecutableDirective(Node);
}

void StatInfInstrStmtPrinter::VisitOMPTargetParallelGenericLoopDirective(
    OMPTargetParallelGenericLoopDirective *Node) {
  Indent() << "#pragma omp target parallel loop";
  PrintOMPExecutableDirective(Node);
}

//===----------------------------------------------------------------------===//
//  Expr printing methods.
//===----------------------------------------------------------------------===//

void StatInfInstrStmtPrinter::VisitSourceLocExpr(SourceLocExpr *Node) {
  OS << Node->getBuiltinStr() << "()";
}

void StatInfInstrStmtPrinter::VisitConstantExpr(ConstantExpr *Node) {
  PrintExpr(Node->getSubExpr());
}

void StatInfInstrStmtPrinter::VisitDeclRefExpr(DeclRefExpr *Node) {
  if (const auto *OCED = dyn_cast<OMPCapturedExprDecl>(Node->getDecl())) {
    OCED->getInit()->IgnoreImpCasts()->printPretty(OS, nullptr, Policy);
    return;
  }
  if (const auto *TPOD = dyn_cast<TemplateParamObjectDecl>(Node->getDecl())) {
    TPOD->printAsExpr(OS, Policy);
    return;
  }
  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(OS, Policy);
  if (Node->hasTemplateKeyword())
    OS << "template ";
  if (Policy.CleanUglifiedParameters &&
      isa<ParmVarDecl, NonTypeTemplateParmDecl>(Node->getDecl()) &&
      Node->getDecl()->getIdentifier())
    OS << Node->getDecl()->getIdentifier()->deuglifiedName();
  else
    Node->getNameInfo().printName(OS, Policy);
  if (Node->hasExplicitTemplateArgs()) {
    const TemplateParameterList *TPL = nullptr;
    if (!Node->hadMultipleCandidates())
      if (auto *TD = dyn_cast<TemplateDecl>(Node->getDecl()))
        TPL = TD->getTemplateParameters();
    printTemplateArgumentList(OS, Node->template_arguments(), Policy, TPL);
  }
}

void StatInfInstrStmtPrinter::VisitDependentScopeDeclRefExpr(
                                           DependentScopeDeclRefExpr *Node) {
  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(OS, Policy);
  if (Node->hasTemplateKeyword())
    OS << "template ";
  OS << Node->getNameInfo();
  if (Node->hasExplicitTemplateArgs())
    printTemplateArgumentList(OS, Node->template_arguments(), Policy);
}

void StatInfInstrStmtPrinter::VisitUnresolvedLookupExpr(UnresolvedLookupExpr *Node) {
  if (Node->getQualifier())
    Node->getQualifier()->print(OS, Policy);
  if (Node->hasTemplateKeyword())
    OS << "template ";
  OS << Node->getNameInfo();
  if (Node->hasExplicitTemplateArgs())
    printTemplateArgumentList(OS, Node->template_arguments(), Policy);
}

static bool isImplicitSelf(const Expr *E) {
  if (const auto *DRE = dyn_cast<DeclRefExpr>(E)) {
    if (const auto *PD = dyn_cast<ImplicitParamDecl>(DRE->getDecl())) {
      if (PD->getParameterKind() == ImplicitParamDecl::ObjCSelf &&
          DRE->getBeginLoc().isInvalid())
        return true;
    }
  }
  return false;
}

void StatInfInstrStmtPrinter::VisitObjCIvarRefExpr(ObjCIvarRefExpr *Node) {
  if (Node->getBase()) {
    if (!Policy.SuppressImplicitBase ||
        !isImplicitSelf(Node->getBase()->IgnoreImpCasts())) {
      PrintExpr(Node->getBase());
      OS << (Node->isArrow() ? "->" : ".");
    }
  }
  OS << *Node->getDecl();
}

void StatInfInstrStmtPrinter::VisitObjCPropertyRefExpr(ObjCPropertyRefExpr *Node) {
  if (Node->isSuperReceiver())
    OS << "super.";
  else if (Node->isObjectReceiver() && Node->getBase()) {
    PrintExpr(Node->getBase());
    OS << ".";
  } else if (Node->isClassReceiver() && Node->getClassReceiver()) {
    OS << Node->getClassReceiver()->getName() << ".";
  }

  if (Node->isImplicitProperty()) {
    if (const auto *Getter = Node->getImplicitPropertyGetter())
      Getter->getSelector().print(OS);
    else
      OS << SelectorTable::getPropertyNameFromSetterSelector(
          Node->getImplicitPropertySetter()->getSelector());
  } else
    OS << Node->getExplicitProperty()->getName();
}

void StatInfInstrStmtPrinter::VisitObjCSubscriptRefExpr(ObjCSubscriptRefExpr *Node) {
  PrintExpr(Node->getBaseExpr());
  OS << "[";
  PrintExpr(Node->getKeyExpr());
  OS << "]";
}

void StatInfInstrStmtPrinter::VisitSYCLUniqueStableNameExpr(
    SYCLUniqueStableNameExpr *Node) {
  OS << "__builtin_sycl_unique_stable_name(";
  Node->getTypeSourceInfo()->getType().print(OS, Policy);
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitPredefinedExpr(PredefinedExpr *Node) {
  OS << PredefinedExpr::getIdentKindName(Node->getIdentKind());
}

void StatInfInstrStmtPrinter::VisitCharacterLiteral(CharacterLiteral *Node) {
  CharacterLiteral::print(Node->getValue(), Node->getKind(), OS);
}

/// Prints the given expression using the original source text. Returns true on
/// success, false otherwise.
static bool printExprAsWritten(raw_ostream &OS, Expr *E,
                               const ASTContext *Context) {
  if (!Context)
    return false;
  bool Invalid = false;
  StringRef Source = Lexer::getSourceText(
      CharSourceRange::getTokenRange(E->getSourceRange()),
      Context->getSourceManager(), Context->getLangOpts(), &Invalid);
  if (!Invalid) {
    OS << Source;
    return true;
  }
  return false;
}

void StatInfInstrStmtPrinter::VisitIntegerLiteral(IntegerLiteral *Node) {
  if (Policy.ConstantsAsWritten && printExprAsWritten(OS, Node, Context))
    return;
  bool isSigned = Node->getType()->isSignedIntegerType();
  OS << toString(Node->getValue(), 10, isSigned);

  if (isa<BitIntType>(Node->getType())) {
    OS << (isSigned ? "wb" : "uwb");
    return;
  }

  // Emit suffixes.  Integer literals are always a builtin integer type.
  switch (Node->getType()->castAs<BuiltinType>()->getKind()) {
  default: llvm_unreachable("Unexpected type for integer literal!");
  case BuiltinType::Char_S:
  case BuiltinType::Char_U:    OS << "i8"; break;
  case BuiltinType::UChar:     OS << "Ui8"; break;
  case BuiltinType::SChar:     OS << "i8"; break;
  case BuiltinType::Short:     OS << "i16"; break;
  case BuiltinType::UShort:    OS << "Ui16"; break;
  case BuiltinType::Int:       break; // no suffix.
  case BuiltinType::UInt:      OS << 'U'; break;
  case BuiltinType::Long:      OS << 'L'; break;
  case BuiltinType::ULong:     OS << "UL"; break;
  case BuiltinType::LongLong:  OS << "LL"; break;
  case BuiltinType::ULongLong: OS << "ULL"; break;
  case BuiltinType::Int128:
    break; // no suffix.
  case BuiltinType::UInt128:
    break; // no suffix.
  case BuiltinType::WChar_S:
  case BuiltinType::WChar_U:
    break; // no suffix
  }
}

void StatInfInstrStmtPrinter::VisitFixedPointLiteral(FixedPointLiteral *Node) {
  if (Policy.ConstantsAsWritten && printExprAsWritten(OS, Node, Context))
    return;
  OS << Node->getValueAsString(/*Radix=*/10);

  switch (Node->getType()->castAs<BuiltinType>()->getKind()) {
    default: llvm_unreachable("Unexpected type for fixed point literal!");
    case BuiltinType::ShortFract:   OS << "hr"; break;
    case BuiltinType::ShortAccum:   OS << "hk"; break;
    case BuiltinType::UShortFract:  OS << "uhr"; break;
    case BuiltinType::UShortAccum:  OS << "uhk"; break;
    case BuiltinType::Fract:        OS << "r"; break;
    case BuiltinType::Accum:        OS << "k"; break;
    case BuiltinType::UFract:       OS << "ur"; break;
    case BuiltinType::UAccum:       OS << "uk"; break;
    case BuiltinType::LongFract:    OS << "lr"; break;
    case BuiltinType::LongAccum:    OS << "lk"; break;
    case BuiltinType::ULongFract:   OS << "ulr"; break;
    case BuiltinType::ULongAccum:   OS << "ulk"; break;
  }
}

static void PrintFloatingLiteral(raw_ostream &OS, FloatingLiteral *Node,
                                 bool PrintSuffix) {
  SmallString<16> Str;
  Node->getValue().toString(Str);
  OS << Str;
  if (Str.find_first_not_of("-0123456789") == StringRef::npos)
    OS << '.'; // Trailing dot in order to separate from ints.

  if (!PrintSuffix)
    return;

  // Emit suffixes.  Float literals are always a builtin float type.
  switch (Node->getType()->castAs<BuiltinType>()->getKind()) {
  default: llvm_unreachable("Unexpected type for float literal!");
  case BuiltinType::Half:       break; // FIXME: suffix?
  case BuiltinType::Ibm128:     break; // FIXME: No suffix for ibm128 literal
  case BuiltinType::Double:     break; // no suffix.
  case BuiltinType::Float16:    OS << "F16"; break;
  case BuiltinType::Float:      OS << 'F'; break;
  case BuiltinType::LongDouble: OS << 'L'; break;
  case BuiltinType::Float128:   OS << 'Q'; break;
  }
}

void StatInfInstrStmtPrinter::VisitFloatingLiteral(FloatingLiteral *Node) {
  if (Policy.ConstantsAsWritten && printExprAsWritten(OS, Node, Context))
    return;
  PrintFloatingLiteral(OS, Node, /*PrintSuffix=*/true);
}

void StatInfInstrStmtPrinter::VisitImaginaryLiteral(ImaginaryLiteral *Node) {
  PrintExpr(Node->getSubExpr());
  OS << "i";
}

void StatInfInstrStmtPrinter::VisitStringLiteral(StringLiteral *Str) {
  Str->outputString(OS);
}

void StatInfInstrStmtPrinter::VisitParenExpr(ParenExpr *Node) {
  OS << "(";
  PrintExpr(Node->getSubExpr());
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitUnaryOperator(UnaryOperator *Node) {
  if (!Node->isPostfix()) {
    OS << UnaryOperator::getOpcodeStr(Node->getOpcode());

    // Print a space if this is an "identifier operator" like __real, or if
    // it might be concatenated incorrectly like '+'.
    switch (Node->getOpcode()) {
    default: break;
    case UO_Real:
    case UO_Imag:
    case UO_Extension:
      OS << ' ';
      break;
    case UO_Plus:
    case UO_Minus:
      if (isa<UnaryOperator>(Node->getSubExpr()))
        OS << ' ';
      break;
    }
  }
  PrintExpr(Node->getSubExpr());

  if (Node->isPostfix())
    OS << UnaryOperator::getOpcodeStr(Node->getOpcode());
}

void StatInfInstrStmtPrinter::VisitOffsetOfExpr(OffsetOfExpr *Node) {
  OS << "__builtin_offsetof(";
  Node->getTypeSourceInfo()->getType().print(OS, Policy);
  OS << ", ";
  bool PrintedSomething = false;
  for (unsigned i = 0, n = Node->getNumComponents(); i < n; ++i) {
    OffsetOfNode ON = Node->getComponent(i);
    if (ON.getKind() == OffsetOfNode::Array) {
      // Array node
      OS << "[";
      PrintExpr(Node->getIndexExpr(ON.getArrayExprIndex()));
      OS << "]";
      PrintedSomething = true;
      continue;
    }

    // Skip implicit base indirections.
    if (ON.getKind() == OffsetOfNode::Base)
      continue;

    // Field or identifier node.
    IdentifierInfo *Id = ON.getFieldName();
    if (!Id)
      continue;

    if (PrintedSomething)
      OS << ".";
    else
      PrintedSomething = true;
    OS << Id->getName();
  }
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitUnaryExprOrTypeTraitExpr(
    UnaryExprOrTypeTraitExpr *Node) {
  const char *Spelling = getTraitSpelling(Node->getKind());
  if (Node->getKind() == UETT_AlignOf) {
    if (Policy.Alignof)
      Spelling = "alignof";
    else if (Policy.UnderscoreAlignof)
      Spelling = "_Alignof";
    else
      Spelling = "__alignof";
  }

  OS << Spelling;

  if (Node->isArgumentType()) {
    OS << '(';
    Node->getArgumentType().print(OS, Policy);
    OS << ')';
  } else {
    OS << " ";
    PrintExpr(Node->getArgumentExpr());
  }
}

void StatInfInstrStmtPrinter::VisitGenericSelectionExpr(GenericSelectionExpr *Node) {
  OS << "_Generic(";
  PrintExpr(Node->getControllingExpr());
  for (const GenericSelectionExpr::Association Assoc : Node->associations()) {
    OS << ", ";
    QualType T = Assoc.getType();
    if (T.isNull())
      OS << "default";
    else
      T.print(OS, Policy);
    OS << ": ";
    PrintExpr(Assoc.getAssociationExpr());
  }
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitArraySubscriptExpr(ArraySubscriptExpr *Node) {
  PrintExpr(Node->getLHS());
  OS << "[";
  PrintExpr(Node->getRHS());
  OS << "]";
}

void StatInfInstrStmtPrinter::VisitMatrixSubscriptExpr(MatrixSubscriptExpr *Node) {
  PrintExpr(Node->getBase());
  OS << "[";
  PrintExpr(Node->getRowIdx());
  OS << "]";
  OS << "[";
  PrintExpr(Node->getColumnIdx());
  OS << "]";
}

void StatInfInstrStmtPrinter::VisitOMPArraySectionExpr(OMPArraySectionExpr *Node) {
  PrintExpr(Node->getBase());
  OS << "[";
  if (Node->getLowerBound())
    PrintExpr(Node->getLowerBound());
  if (Node->getColonLocFirst().isValid()) {
    OS << ":";
    if (Node->getLength())
      PrintExpr(Node->getLength());
  }
  if (Node->getColonLocSecond().isValid()) {
    OS << ":";
    if (Node->getStride())
      PrintExpr(Node->getStride());
  }
  OS << "]";
}

void StatInfInstrStmtPrinter::VisitOMPArrayShapingExpr(OMPArrayShapingExpr *Node) {
  OS << "(";
  for (Expr *E : Node->getDimensions()) {
    OS << "[";
    PrintExpr(E);
    OS << "]";
  }
  OS << ")";
  PrintExpr(Node->getBase());
}

void StatInfInstrStmtPrinter::VisitOMPIteratorExpr(OMPIteratorExpr *Node) {
  OS << "iterator(";
  for (unsigned I = 0, E = Node->numOfIterators(); I < E; ++I) {
    auto *VD = cast<ValueDecl>(Node->getIteratorDecl(I));
    VD->getType().print(OS, Policy);
    const OMPIteratorExpr::IteratorRange Range = Node->getIteratorRange(I);
    OS << " " << VD->getName() << " = ";
    PrintExpr(Range.Begin);
    OS << ":";
    PrintExpr(Range.End);
    if (Range.Step) {
      OS << ":";
      PrintExpr(Range.Step);
    }
    if (I < E - 1)
      OS << ", ";
  }
  OS << ")";
}

void StatInfInstrStmtPrinter::PrintCallArgs(CallExpr *Call) {
  for (unsigned i = 0, e = Call->getNumArgs(); i != e; ++i) {
    if (isa<CXXDefaultArgExpr>(Call->getArg(i))) {
      // Don't print any defaulted arguments
      break;
    }

    if (i) OS << ", ";
    PrintExpr(Call->getArg(i));
  }
}

void StatInfInstrStmtPrinter::VisitCallExpr(CallExpr *Call) {
  PrintExpr(Call->getCallee());
  OS << "(";
  PrintCallArgs(Call);
  OS << ")";
}

static bool isImplicitThis(const Expr *E) {
  if (const auto *TE = dyn_cast<CXXThisExpr>(E))
    return TE->isImplicit();
  return false;
}

void StatInfInstrStmtPrinter::VisitMemberExpr(MemberExpr *Node) {
  if (!Policy.SuppressImplicitBase || !isImplicitThis(Node->getBase())) {
    PrintExpr(Node->getBase());

    auto *ParentMember = dyn_cast<MemberExpr>(Node->getBase());
    FieldDecl *ParentDecl =
        ParentMember ? dyn_cast<FieldDecl>(ParentMember->getMemberDecl())
                     : nullptr;

    if (!ParentDecl || !ParentDecl->isAnonymousStructOrUnion())
      OS << (Node->isArrow() ? "->" : ".");
  }

  if (auto *FD = dyn_cast<FieldDecl>(Node->getMemberDecl()))
    if (FD->isAnonymousStructOrUnion())
      return;

  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(OS, Policy);
  if (Node->hasTemplateKeyword())
    OS << "template ";
  OS << Node->getMemberNameInfo();
  const TemplateParameterList *TPL = nullptr;
  if (auto *FD = dyn_cast<FunctionDecl>(Node->getMemberDecl())) {
    if (!Node->hadMultipleCandidates())
      if (auto *FTD = FD->getPrimaryTemplate())
        TPL = FTD->getTemplateParameters();
  } else if (auto *VTSD =
                 dyn_cast<VarTemplateSpecializationDecl>(Node->getMemberDecl()))
    TPL = VTSD->getSpecializedTemplate()->getTemplateParameters();
  if (Node->hasExplicitTemplateArgs())
    printTemplateArgumentList(OS, Node->template_arguments(), Policy, TPL);
}

void StatInfInstrStmtPrinter::VisitObjCIsaExpr(ObjCIsaExpr *Node) {
  PrintExpr(Node->getBase());
  OS << (Node->isArrow() ? "->isa" : ".isa");
}

void StatInfInstrStmtPrinter::VisitExtVectorElementExpr(ExtVectorElementExpr *Node) {
  PrintExpr(Node->getBase());
  OS << ".";
  OS << Node->getAccessor().getName();
}

void StatInfInstrStmtPrinter::VisitCStyleCastExpr(CStyleCastExpr *Node) {
  OS << '(';
  Node->getTypeAsWritten().print(OS, Policy);
  OS << ')';
  PrintExpr(Node->getSubExpr());
}

void StatInfInstrStmtPrinter::VisitCompoundLiteralExpr(CompoundLiteralExpr *Node) {
  OS << '(';
  Node->getType().print(OS, Policy);
  OS << ')';
  PrintExpr(Node->getInitializer());
}

void StatInfInstrStmtPrinter::VisitImplicitCastExpr(ImplicitCastExpr *Node) {
  // No need to print anything, simply forward to the subexpression.
  PrintExpr(Node->getSubExpr());
}

void StatInfInstrStmtPrinter::VisitBinaryOperator(BinaryOperator *Node) {
  PrintExpr(Node->getLHS());
  OS << " " << BinaryOperator::getOpcodeStr(Node->getOpcode()) << " ";
  PrintExpr(Node->getRHS());
}

void StatInfInstrStmtPrinter::VisitCompoundAssignOperator(CompoundAssignOperator *Node) {
  PrintExpr(Node->getLHS());
  OS << " " << BinaryOperator::getOpcodeStr(Node->getOpcode()) << " ";
  PrintExpr(Node->getRHS());
}

void StatInfInstrStmtPrinter::VisitConditionalOperator(ConditionalOperator *Node) {
  PrintExpr(Node->getCond());
  OS << " ? ";
  PrintExpr(Node->getLHS());
  OS << " : ";
  PrintExpr(Node->getRHS());
}

// GNU extensions.

void
StatInfInstrStmtPrinter::VisitBinaryConditionalOperator(BinaryConditionalOperator *Node) {
  PrintExpr(Node->getCommon());
  OS << " ?: ";
  PrintExpr(Node->getFalseExpr());
}

void StatInfInstrStmtPrinter::VisitAddrLabelExpr(AddrLabelExpr *Node) {
  OS << "&&" << Node->getLabel()->getName();
}

void StatInfInstrStmtPrinter::VisitStmtExpr(StmtExpr *E) {
  OS << "(";
  PrintRawCompoundStmt(E->getSubStmt());
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitChooseExpr(ChooseExpr *Node) {
  OS << "__builtin_choose_expr(";
  PrintExpr(Node->getCond());
  OS << ", ";
  PrintExpr(Node->getLHS());
  OS << ", ";
  PrintExpr(Node->getRHS());
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitGNUNullExpr(GNUNullExpr *) {
  OS << "__null";
}

void StatInfInstrStmtPrinter::VisitShuffleVectorExpr(ShuffleVectorExpr *Node) {
  OS << "__builtin_shufflevector(";
  for (unsigned i = 0, e = Node->getNumSubExprs(); i != e; ++i) {
    if (i) OS << ", ";
    PrintExpr(Node->getExpr(i));
  }
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitConvertVectorExpr(ConvertVectorExpr *Node) {
  OS << "__builtin_convertvector(";
  PrintExpr(Node->getSrcExpr());
  OS << ", ";
  Node->getType().print(OS, Policy);
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitInitListExpr(InitListExpr* Node) {
  if (Node->getSyntacticForm()) {
    Visit(Node->getSyntacticForm());
    return;
  }

  OS << "{";
  for (unsigned i = 0, e = Node->getNumInits(); i != e; ++i) {
    if (i) OS << ", ";
    if (Node->getInit(i))
      PrintExpr(Node->getInit(i));
    else
      OS << "{}";
  }
  OS << "}";
}

void StatInfInstrStmtPrinter::VisitArrayInitLoopExpr(ArrayInitLoopExpr *Node) {
  // There's no way to express this expression in any of our supported
  // languages, so just emit something terse and (hopefully) clear.
  OS << "{";
  PrintExpr(Node->getSubExpr());
  OS << "}";
}

void StatInfInstrStmtPrinter::VisitArrayInitIndexExpr(ArrayInitIndexExpr *Node) {
  OS << "*";
}

void StatInfInstrStmtPrinter::VisitParenListExpr(ParenListExpr* Node) {
  OS << "(";
  for (unsigned i = 0, e = Node->getNumExprs(); i != e; ++i) {
    if (i) OS << ", ";
    PrintExpr(Node->getExpr(i));
  }
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitDesignatedInitExpr(DesignatedInitExpr *Node) {
  bool NeedsEquals = true;
  for (const DesignatedInitExpr::Designator &D : Node->designators()) {
    if (D.isFieldDesignator()) {
      if (D.getDotLoc().isInvalid()) {
        if (IdentifierInfo *II = D.getFieldName()) {
          OS << II->getName() << ":";
          NeedsEquals = false;
        }
      } else {
        OS << "." << D.getFieldName()->getName();
      }
    } else {
      OS << "[";
      if (D.isArrayDesignator()) {
        PrintExpr(Node->getArrayIndex(D));
      } else {
        PrintExpr(Node->getArrayRangeStart(D));
        OS << " ... ";
        PrintExpr(Node->getArrayRangeEnd(D));
      }
      OS << "]";
    }
  }

  if (NeedsEquals)
    OS << " = ";
  else
    OS << " ";
  PrintExpr(Node->getInit());
}

void StatInfInstrStmtPrinter::VisitDesignatedInitUpdateExpr(
    DesignatedInitUpdateExpr *Node) {
  OS << "{";
  OS << "/*base*/";
  PrintExpr(Node->getBase());
  OS << ", ";

  OS << "/*updater*/";
  PrintExpr(Node->getUpdater());
  OS << "}";
}

void StatInfInstrStmtPrinter::VisitNoInitExpr(NoInitExpr *Node) {
  OS << "/*no init*/";
}

void StatInfInstrStmtPrinter::VisitImplicitValueInitExpr(ImplicitValueInitExpr *Node) {
  if (Node->getType()->getAsCXXRecordDecl()) {
    OS << "/*implicit*/";
    Node->getType().print(OS, Policy);
    OS << "()";
  } else {
    OS << "/*implicit*/(";
    Node->getType().print(OS, Policy);
    OS << ')';
    if (Node->getType()->isRecordType())
      OS << "{}";
    else
      OS << 0;
  }
}

void StatInfInstrStmtPrinter::VisitVAArgExpr(VAArgExpr *Node) {
  OS << "__builtin_va_arg(";
  PrintExpr(Node->getSubExpr());
  OS << ", ";
  Node->getType().print(OS, Policy);
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitPseudoObjectExpr(PseudoObjectExpr *Node) {
  PrintExpr(Node->getSyntacticForm());
}

void StatInfInstrStmtPrinter::VisitAtomicExpr(AtomicExpr *Node) {
  const char *Name = nullptr;
  switch (Node->getOp()) {
#define BUILTIN(ID, TYPE, ATTRS)
#define ATOMIC_BUILTIN(ID, TYPE, ATTRS) \
  case AtomicExpr::AO ## ID: \
    Name = #ID "("; \
    break;
#include "clang/Basic/Builtins.def"
  }
  OS << Name;

  // AtomicExpr stores its subexpressions in a permuted order.
  PrintExpr(Node->getPtr());
  if (Node->getOp() != AtomicExpr::AO__c11_atomic_load &&
      Node->getOp() != AtomicExpr::AO__atomic_load_n &&
      Node->getOp() != AtomicExpr::AO__opencl_atomic_load &&
      Node->getOp() != AtomicExpr::AO__hip_atomic_load) {
    OS << ", ";
    PrintExpr(Node->getVal1());
  }
  if (Node->getOp() == AtomicExpr::AO__atomic_exchange ||
      Node->isCmpXChg()) {
    OS << ", ";
    PrintExpr(Node->getVal2());
  }
  if (Node->getOp() == AtomicExpr::AO__atomic_compare_exchange ||
      Node->getOp() == AtomicExpr::AO__atomic_compare_exchange_n) {
    OS << ", ";
    PrintExpr(Node->getWeak());
  }
  if (Node->getOp() != AtomicExpr::AO__c11_atomic_init &&
      Node->getOp() != AtomicExpr::AO__opencl_atomic_init) {
    OS << ", ";
    PrintExpr(Node->getOrder());
  }
  if (Node->isCmpXChg()) {
    OS << ", ";
    PrintExpr(Node->getOrderFail());
  }
  OS << ")";
}

// C++
void StatInfInstrStmtPrinter::VisitCXXOperatorCallExpr(CXXOperatorCallExpr *Node) {
  OverloadedOperatorKind Kind = Node->getOperator();
  if (Kind == OO_PlusPlus || Kind == OO_MinusMinus) {
    if (Node->getNumArgs() == 1) {
      OS << getOperatorSpelling(Kind) << ' ';
      PrintExpr(Node->getArg(0));
    } else {
      PrintExpr(Node->getArg(0));
      OS << ' ' << getOperatorSpelling(Kind);
    }
  } else if (Kind == OO_Arrow) {
    PrintExpr(Node->getArg(0));
  } else if (Kind == OO_Call || Kind == OO_Subscript) {
    PrintExpr(Node->getArg(0));
    OS << (Kind == OO_Call ? '(' : '[');
    for (unsigned ArgIdx = 1; ArgIdx < Node->getNumArgs(); ++ArgIdx) {
      if (ArgIdx > 1)
        OS << ", ";
      if (!isa<CXXDefaultArgExpr>(Node->getArg(ArgIdx)))
        PrintExpr(Node->getArg(ArgIdx));
    }
    OS << (Kind == OO_Call ? ')' : ']');
  } else if (Node->getNumArgs() == 1) {
    OS << getOperatorSpelling(Kind) << ' ';
    PrintExpr(Node->getArg(0));
  } else if (Node->getNumArgs() == 2) {
    PrintExpr(Node->getArg(0));
    OS << ' ' << getOperatorSpelling(Kind) << ' ';
    PrintExpr(Node->getArg(1));
  } else {
    llvm_unreachable("unknown overloaded operator");
  }
}

void StatInfInstrStmtPrinter::VisitCXXMemberCallExpr(CXXMemberCallExpr *Node) {
  // If we have a conversion operator call only print the argument.
  CXXMethodDecl *MD = Node->getMethodDecl();
  if (MD && isa<CXXConversionDecl>(MD)) {
    PrintExpr(Node->getImplicitObjectArgument());
    return;
  }
  VisitCallExpr(cast<CallExpr>(Node));
}

void StatInfInstrStmtPrinter::VisitCUDAKernelCallExpr(CUDAKernelCallExpr *Node) {
  PrintExpr(Node->getCallee());
  OS << "<<<";
  PrintCallArgs(Node->getConfig());
  OS << ">>>(";
  PrintCallArgs(Node);
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitCXXRewrittenBinaryOperator(
    CXXRewrittenBinaryOperator *Node) {
  CXXRewrittenBinaryOperator::DecomposedForm Decomposed =
      Node->getDecomposedForm();
  PrintExpr(const_cast<Expr*>(Decomposed.LHS));
  OS << ' ' << BinaryOperator::getOpcodeStr(Decomposed.Opcode) << ' ';
  PrintExpr(const_cast<Expr*>(Decomposed.RHS));
}

void StatInfInstrStmtPrinter::VisitCXXNamedCastExpr(CXXNamedCastExpr *Node) {
  OS << Node->getCastName() << '<';
  Node->getTypeAsWritten().print(OS, Policy);
  OS << ">(";
  PrintExpr(Node->getSubExpr());
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitCXXStaticCastExpr(CXXStaticCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfInstrStmtPrinter::VisitCXXDynamicCastExpr(CXXDynamicCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfInstrStmtPrinter::VisitCXXReinterpretCastExpr(CXXReinterpretCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfInstrStmtPrinter::VisitCXXConstCastExpr(CXXConstCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfInstrStmtPrinter::VisitBuiltinBitCastExpr(BuiltinBitCastExpr *Node) {
  OS << "__builtin_bit_cast(";
  Node->getTypeInfoAsWritten()->getType().print(OS, Policy);
  OS << ", ";
  PrintExpr(Node->getSubExpr());
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitCXXAddrspaceCastExpr(CXXAddrspaceCastExpr *Node) {
  VisitCXXNamedCastExpr(Node);
}

void StatInfInstrStmtPrinter::VisitCXXTypeidExpr(CXXTypeidExpr *Node) {
  OS << "typeid(";
  if (Node->isTypeOperand()) {
    Node->getTypeOperandSourceInfo()->getType().print(OS, Policy);
  } else {
    PrintExpr(Node->getExprOperand());
  }
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitCXXUuidofExpr(CXXUuidofExpr *Node) {
  OS << "__uuidof(";
  if (Node->isTypeOperand()) {
    Node->getTypeOperandSourceInfo()->getType().print(OS, Policy);
  } else {
    PrintExpr(Node->getExprOperand());
  }
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitMSPropertyRefExpr(MSPropertyRefExpr *Node) {
  PrintExpr(Node->getBaseExpr());
  if (Node->isArrow())
    OS << "->";
  else
    OS << ".";
  if (NestedNameSpecifier *Qualifier =
      Node->getQualifierLoc().getNestedNameSpecifier())
    Qualifier->print(OS, Policy);
  OS << Node->getPropertyDecl()->getDeclName();
}

void StatInfInstrStmtPrinter::VisitMSPropertySubscriptExpr(MSPropertySubscriptExpr *Node) {
  PrintExpr(Node->getBase());
  OS << "[";
  PrintExpr(Node->getIdx());
  OS << "]";
}

void StatInfInstrStmtPrinter::VisitUserDefinedLiteral(UserDefinedLiteral *Node) {
  switch (Node->getLiteralOperatorKind()) {
  case UserDefinedLiteral::LOK_Raw:
    OS << cast<StringLiteral>(Node->getArg(0)->IgnoreImpCasts())->getString();
    break;
  case UserDefinedLiteral::LOK_Template: {
    const auto *DRE = cast<DeclRefExpr>(Node->getCallee()->IgnoreImpCasts());
    const TemplateArgumentList *Args =
      cast<FunctionDecl>(DRE->getDecl())->getTemplateSpecializationArgs();
    assert(Args);

    if (Args->size() != 1 || Args->get(0).getKind() != TemplateArgument::Pack) {
      const TemplateParameterList *TPL = nullptr;
      if (!DRE->hadMultipleCandidates())
        if (const auto *TD = dyn_cast<TemplateDecl>(DRE->getDecl()))
          TPL = TD->getTemplateParameters();
      OS << "operator\"\"" << Node->getUDSuffix()->getName();
      printTemplateArgumentList(OS, Args->asArray(), Policy, TPL);
      OS << "()";
      return;
    }

    const TemplateArgument &Pack = Args->get(0);
    for (const auto &P : Pack.pack_elements()) {
      char C = (char)P.getAsIntegral().getZExtValue();
      OS << C;
    }
    break;
  }
  case UserDefinedLiteral::LOK_Integer: {
    // Print integer literal without suffix.
    const auto *Int = cast<IntegerLiteral>(Node->getCookedLiteral());
    OS << toString(Int->getValue(), 10, /*isSigned*/false);
    break;
  }
  case UserDefinedLiteral::LOK_Floating: {
    // Print floating literal without suffix.
    auto *Float = cast<FloatingLiteral>(Node->getCookedLiteral());
    PrintFloatingLiteral(OS, Float, /*PrintSuffix=*/false);
    break;
  }
  case UserDefinedLiteral::LOK_String:
  case UserDefinedLiteral::LOK_Character:
    PrintExpr(Node->getCookedLiteral());
    break;
  }
  OS << Node->getUDSuffix()->getName();
}

void StatInfInstrStmtPrinter::VisitCXXBoolLiteralExpr(CXXBoolLiteralExpr *Node) {
  OS << (Node->getValue() ? "true" : "false");
}

void StatInfInstrStmtPrinter::VisitCXXNullPtrLiteralExpr(CXXNullPtrLiteralExpr *Node) {
  OS << "nullptr";
}

void StatInfInstrStmtPrinter::VisitCXXThisExpr(CXXThisExpr *Node) {
  OS << "this";
}

void StatInfInstrStmtPrinter::VisitCXXThrowExpr(CXXThrowExpr *Node) {
  if (!Node->getSubExpr())
    OS << "throw";
  else {
    OS << "throw ";
    PrintExpr(Node->getSubExpr());
  }
}

void StatInfInstrStmtPrinter::VisitCXXDefaultArgExpr(CXXDefaultArgExpr *Node) {
  // Nothing to print: we picked up the default argument.
}

void StatInfInstrStmtPrinter::VisitCXXDefaultInitExpr(CXXDefaultInitExpr *Node) {
  // Nothing to print: we picked up the default initializer.
}

void StatInfInstrStmtPrinter::VisitCXXFunctionalCastExpr(CXXFunctionalCastExpr *Node) {
  auto TargetType = Node->getType();
  auto *Auto = TargetType->getContainedDeducedType();
  bool Bare = Auto && Auto->isDeduced();

  // Parenthesize deduced casts.
  if (Bare)
    OS << '(';
  TargetType.print(OS, Policy);
  if (Bare)
    OS << ')';

  // No extra braces surrounding the inner construct.
  if (!Node->isListInitialization())
    OS << '(';
  PrintExpr(Node->getSubExpr());
  if (!Node->isListInitialization())
    OS << ')';
}

void StatInfInstrStmtPrinter::VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *Node) {
  PrintExpr(Node->getSubExpr());
}

void StatInfInstrStmtPrinter::VisitCXXTemporaryObjectExpr(CXXTemporaryObjectExpr *Node) {
  Node->getType().print(OS, Policy);
  if (Node->isStdInitListInitialization())
    /* Nothing to do; braces are part of creating the std::initializer_list. */;
  else if (Node->isListInitialization())
    OS << "{";
  else
    OS << "(";
  for (CXXTemporaryObjectExpr::arg_iterator Arg = Node->arg_begin(),
                                         ArgEnd = Node->arg_end();
       Arg != ArgEnd; ++Arg) {
    if ((*Arg)->isDefaultArgument())
      break;
    if (Arg != Node->arg_begin())
      OS << ", ";
    PrintExpr(*Arg);
  }
  if (Node->isStdInitListInitialization())
    /* See above. */;
  else if (Node->isListInitialization())
    OS << "}";
  else
    OS << ")";
}

void StatInfInstrStmtPrinter::VisitLambdaExpr(LambdaExpr *Node) {
  OS << '[';
  bool NeedComma = false;
  switch (Node->getCaptureDefault()) {
  case LCD_None:
    break;

  case LCD_ByCopy:
    OS << '=';
    NeedComma = true;
    break;

  case LCD_ByRef:
    OS << '&';
    NeedComma = true;
    break;
  }
  for (LambdaExpr::capture_iterator C = Node->explicit_capture_begin(),
                                 CEnd = Node->explicit_capture_end();
       C != CEnd;
       ++C) {
    if (C->capturesVLAType())
      continue;

    if (NeedComma)
      OS << ", ";
    NeedComma = true;

    switch (C->getCaptureKind()) {
    case LCK_This:
      OS << "this";
      break;

    case LCK_StarThis:
      OS << "*this";
      break;

    case LCK_ByRef:
      if (Node->getCaptureDefault() != LCD_ByRef || Node->isInitCapture(C))
        OS << '&';
      OS << C->getCapturedVar()->getName();
      break;

    case LCK_ByCopy:
      OS << C->getCapturedVar()->getName();
      break;

    case LCK_VLAType:
      llvm_unreachable("VLA type in explicit captures.");
    }

    if (C->isPackExpansion())
      OS << "...";

    if (Node->isInitCapture(C)) {
      // Init captures are always VarDecl.
      auto *D = cast<VarDecl>(C->getCapturedVar());

      llvm::StringRef Pre;
      llvm::StringRef Post;
      if (D->getInitStyle() == VarDecl::CallInit &&
          !isa<ParenListExpr>(D->getInit())) {
        Pre = "(";
        Post = ")";
      } else if (D->getInitStyle() == VarDecl::CInit) {
        Pre = " = ";
      }

      OS << Pre;
      PrintExpr(D->getInit());
      OS << Post;
    }
  }
  OS << ']';

  if (!Node->getExplicitTemplateParameters().empty()) {
    Node->getTemplateParameterList()->print(
        OS, Node->getLambdaClass()->getASTContext(),
        /*OmitTemplateKW*/true);
  }

  if (Node->hasExplicitParameters()) {
    OS << '(';
    CXXMethodDecl *Method = Node->getCallOperator();
    NeedComma = false;
    for (const auto *P : Method->parameters()) {
      if (NeedComma) {
        OS << ", ";
      } else {
        NeedComma = true;
      }
      std::string ParamStr =
          (Policy.CleanUglifiedParameters && P->getIdentifier())
              ? P->getIdentifier()->deuglifiedName().str()
              : P->getNameAsString();
      P->getOriginalType().print(OS, Policy, ParamStr);
    }
    if (Method->isVariadic()) {
      if (NeedComma)
        OS << ", ";
      OS << "...";
    }
    OS << ')';

    if (Node->isMutable())
      OS << " mutable";

    auto *Proto = Method->getType()->castAs<FunctionProtoType>();
    Proto->printExceptionSpecification(OS, Policy);

    // FIXME: Attributes

    // Print the trailing return type if it was specified in the source.
    if (Node->hasExplicitResultType()) {
      OS << " -> ";
      Proto->getReturnType().print(OS, Policy);
    }
  }

  // Print the body.
  OS << ' ';
  if (Policy.TerseOutput)
    OS << "{}";
  else
    PrintRawCompoundStmt(Node->getCompoundStmtBody());
}

void StatInfInstrStmtPrinter::VisitCXXScalarValueInitExpr(CXXScalarValueInitExpr *Node) {
  if (TypeSourceInfo *TSInfo = Node->getTypeSourceInfo())
    TSInfo->getType().print(OS, Policy);
  else
    Node->getType().print(OS, Policy);
  OS << "()";
}

void StatInfInstrStmtPrinter::VisitCXXNewExpr(CXXNewExpr *E) {
  if (E->isGlobalNew())
    OS << "::";
  OS << "new ";
  unsigned NumPlace = E->getNumPlacementArgs();
  if (NumPlace > 0 && !isa<CXXDefaultArgExpr>(E->getPlacementArg(0))) {
    OS << "(";
    PrintExpr(E->getPlacementArg(0));
    for (unsigned i = 1; i < NumPlace; ++i) {
      if (isa<CXXDefaultArgExpr>(E->getPlacementArg(i)))
        break;
      OS << ", ";
      PrintExpr(E->getPlacementArg(i));
    }
    OS << ") ";
  }
  if (E->isParenTypeId())
    OS << "(";
  std::string TypeS;
  if (E->isArray()) {
    llvm::raw_string_ostream s(TypeS);
    s << '[';
    if (std::optional<Expr *> Size = E->getArraySize())
      (*Size)->printPretty(s, Helper, Policy);
    s << ']';
  }
  E->getAllocatedType().print(OS, Policy, TypeS);
  if (E->isParenTypeId())
    OS << ")";

  CXXNewExpr::InitializationStyle InitStyle = E->getInitializationStyle();
  if (InitStyle != CXXNewExpr::NoInit) {
    bool Bare = InitStyle == CXXNewExpr::CallInit &&
                !isa<ParenListExpr>(E->getInitializer());
    if (Bare)
      OS << "(";
    PrintExpr(E->getInitializer());
    if (Bare)
      OS << ")";
  }
}

void StatInfInstrStmtPrinter::VisitCXXDeleteExpr(CXXDeleteExpr *E) {
  if (E->isGlobalDelete())
    OS << "::";
  OS << "delete ";
  if (E->isArrayForm())
    OS << "[] ";
  PrintExpr(E->getArgument());
}

void StatInfInstrStmtPrinter::VisitCXXPseudoDestructorExpr(CXXPseudoDestructorExpr *E) {
  PrintExpr(E->getBase());
  if (E->isArrow())
    OS << "->";
  else
    OS << '.';
  if (E->getQualifier())
    E->getQualifier()->print(OS, Policy);
  OS << "~";

  if (IdentifierInfo *II = E->getDestroyedTypeIdentifier())
    OS << II->getName();
  else
    E->getDestroyedType().print(OS, Policy);
}

void StatInfInstrStmtPrinter::VisitCXXConstructExpr(CXXConstructExpr *E) {
  if (E->isListInitialization() && !E->isStdInitListInitialization())
    OS << "{";

  for (unsigned i = 0, e = E->getNumArgs(); i != e; ++i) {
    if (isa<CXXDefaultArgExpr>(E->getArg(i))) {
      // Don't print any defaulted arguments
      break;
    }

    if (i) OS << ", ";
    PrintExpr(E->getArg(i));
  }

  if (E->isListInitialization() && !E->isStdInitListInitialization())
    OS << "}";
}

void StatInfInstrStmtPrinter::VisitCXXInheritedCtorInitExpr(CXXInheritedCtorInitExpr *E) {
  // Parens are printed by the surrounding context.
  OS << "<forwarded>";
}

void StatInfInstrStmtPrinter::VisitCXXStdInitializerListExpr(CXXStdInitializerListExpr *E) {
  PrintExpr(E->getSubExpr());
}

void StatInfInstrStmtPrinter::VisitExprWithCleanups(ExprWithCleanups *E) {
  // Just forward to the subexpression.
  PrintExpr(E->getSubExpr());
}

void StatInfInstrStmtPrinter::VisitCXXUnresolvedConstructExpr(
    CXXUnresolvedConstructExpr *Node) {
  Node->getTypeAsWritten().print(OS, Policy);
  if (!Node->isListInitialization())
    OS << '(';
  for (auto Arg = Node->arg_begin(), ArgEnd = Node->arg_end(); Arg != ArgEnd;
       ++Arg) {
    if (Arg != Node->arg_begin())
      OS << ", ";
    PrintExpr(*Arg);
  }
  if (!Node->isListInitialization())
    OS << ')';
}

void StatInfInstrStmtPrinter::VisitCXXDependentScopeMemberExpr(
                                         CXXDependentScopeMemberExpr *Node) {
  if (!Node->isImplicitAccess()) {
    PrintExpr(Node->getBase());
    OS << (Node->isArrow() ? "->" : ".");
  }
  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(OS, Policy);
  if (Node->hasTemplateKeyword())
    OS << "template ";
  OS << Node->getMemberNameInfo();
  if (Node->hasExplicitTemplateArgs())
    printTemplateArgumentList(OS, Node->template_arguments(), Policy);
}

void StatInfInstrStmtPrinter::VisitUnresolvedMemberExpr(UnresolvedMemberExpr *Node) {
  if (!Node->isImplicitAccess()) {
    PrintExpr(Node->getBase());
    OS << (Node->isArrow() ? "->" : ".");
  }
  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(OS, Policy);
  if (Node->hasTemplateKeyword())
    OS << "template ";
  OS << Node->getMemberNameInfo();
  if (Node->hasExplicitTemplateArgs())
    printTemplateArgumentList(OS, Node->template_arguments(), Policy);
}

void StatInfInstrStmtPrinter::VisitTypeTraitExpr(TypeTraitExpr *E) {
  OS << getTraitSpelling(E->getTrait()) << "(";
  for (unsigned I = 0, N = E->getNumArgs(); I != N; ++I) {
    if (I > 0)
      OS << ", ";
    E->getArg(I)->getType().print(OS, Policy);
  }
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitArrayTypeTraitExpr(ArrayTypeTraitExpr *E) {
  OS << getTraitSpelling(E->getTrait()) << '(';
  E->getQueriedType().print(OS, Policy);
  OS << ')';
}

void StatInfInstrStmtPrinter::VisitExpressionTraitExpr(ExpressionTraitExpr *E) {
  OS << getTraitSpelling(E->getTrait()) << '(';
  PrintExpr(E->getQueriedExpression());
  OS << ')';
}

void StatInfInstrStmtPrinter::VisitCXXNoexceptExpr(CXXNoexceptExpr *E) {
  OS << "noexcept(";
  PrintExpr(E->getOperand());
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitPackExpansionExpr(PackExpansionExpr *E) {
  PrintExpr(E->getPattern());
  OS << "...";
}

void StatInfInstrStmtPrinter::VisitSizeOfPackExpr(SizeOfPackExpr *E) {
  OS << "sizeof...(" << *E->getPack() << ")";
}

void StatInfInstrStmtPrinter::VisitSubstNonTypeTemplateParmPackExpr(
                                       SubstNonTypeTemplateParmPackExpr *Node) {
  OS << *Node->getParameterPack();
}

void StatInfInstrStmtPrinter::VisitSubstNonTypeTemplateParmExpr(
                                       SubstNonTypeTemplateParmExpr *Node) {
  Visit(Node->getReplacement());
}

void StatInfInstrStmtPrinter::VisitFunctionParmPackExpr(FunctionParmPackExpr *E) {
  OS << *E->getParameterPack();
}

void StatInfInstrStmtPrinter::VisitMaterializeTemporaryExpr(MaterializeTemporaryExpr *Node){
  PrintExpr(Node->getSubExpr());
}

void StatInfInstrStmtPrinter::VisitCXXFoldExpr(CXXFoldExpr *E) {
  OS << "(";
  if (E->getLHS()) {
    PrintExpr(E->getLHS());
    OS << " " << BinaryOperator::getOpcodeStr(E->getOperator()) << " ";
  }
  OS << "...";
  if (E->getRHS()) {
    OS << " " << BinaryOperator::getOpcodeStr(E->getOperator()) << " ";
    PrintExpr(E->getRHS());
  }
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitCXXParenListInitExpr(CXXParenListInitExpr *Node) {
  OS << "(";
  llvm::interleaveComma(Node->getInitExprs(), OS,
                        [&](Expr *E) { PrintExpr(E); });
  OS << ")";
}

void StatInfInstrStmtPrinter::VisitConceptSpecializationExpr(ConceptSpecializationExpr *E) {
  NestedNameSpecifierLoc NNS = E->getNestedNameSpecifierLoc();
  if (NNS)
    NNS.getNestedNameSpecifier()->print(OS, Policy);
  if (E->getTemplateKWLoc().isValid())
    OS << "template ";
  OS << E->getFoundDecl()->getName();
  printTemplateArgumentList(OS, E->getTemplateArgsAsWritten()->arguments(),
                            Policy,
                            E->getNamedConcept()->getTemplateParameters());
}

void StatInfInstrStmtPrinter::VisitRequiresExpr(RequiresExpr *E) {
  OS << "requires ";
  auto LocalParameters = E->getLocalParameters();
  if (!LocalParameters.empty()) {
    OS << "(";
    for (ParmVarDecl *LocalParam : LocalParameters) {
      PrintRawDecl(LocalParam);
      if (LocalParam != LocalParameters.back())
        OS << ", ";
    }

    OS << ") ";
  }
  OS << "{ ";
  auto Requirements = E->getRequirements();
  for (concepts::Requirement *Req : Requirements) {
    if (auto *TypeReq = dyn_cast<concepts::TypeRequirement>(Req)) {
      if (TypeReq->isSubstitutionFailure())
        OS << "<<error-type>>";
      else
        TypeReq->getType()->getType().print(OS, Policy);
    } else if (auto *ExprReq = dyn_cast<concepts::ExprRequirement>(Req)) {
      if (ExprReq->isCompound())
        OS << "{ ";
      if (ExprReq->isExprSubstitutionFailure())
        OS << "<<error-expression>>";
      else
        PrintExpr(ExprReq->getExpr());
      if (ExprReq->isCompound()) {
        OS << " }";
        if (ExprReq->getNoexceptLoc().isValid())
          OS << " noexcept";
        const auto &RetReq = ExprReq->getReturnTypeRequirement();
        if (!RetReq.isEmpty()) {
          OS << " -> ";
          if (RetReq.isSubstitutionFailure())
            OS << "<<error-type>>";
          else if (RetReq.isTypeConstraint())
            RetReq.getTypeConstraint()->print(OS, Policy);
        }
      }
    } else {
      auto *NestedReq = cast<concepts::NestedRequirement>(Req);
      OS << "requires ";
      if (NestedReq->hasInvalidConstraint())
        OS << "<<error-expression>>";
      else
        PrintExpr(NestedReq->getConstraintExpr());
    }
    OS << "; ";
  }
  OS << "}";
}

// C++ Coroutines TS

void StatInfInstrStmtPrinter::VisitCoroutineBodyStmt(CoroutineBodyStmt *S) {
  Visit(S->getBody());
}

void StatInfInstrStmtPrinter::VisitCoreturnStmt(CoreturnStmt *S) {
  OS << "co_return";
  if (S->getOperand()) {
    OS << " ";
    Visit(S->getOperand());
  }
  OS << ";";
}

void StatInfInstrStmtPrinter::VisitCoawaitExpr(CoawaitExpr *S) {
  OS << "co_await ";
  PrintExpr(S->getOperand());
}

void StatInfInstrStmtPrinter::VisitDependentCoawaitExpr(DependentCoawaitExpr *S) {
  OS << "co_await ";
  PrintExpr(S->getOperand());
}

void StatInfInstrStmtPrinter::VisitCoyieldExpr(CoyieldExpr *S) {
  OS << "co_yield ";
  PrintExpr(S->getOperand());
}

// Obj-C

void StatInfInstrStmtPrinter::VisitObjCStringLiteral(ObjCStringLiteral *Node) {
  OS << "@";
  VisitStringLiteral(Node->getString());
}

void StatInfInstrStmtPrinter::VisitObjCBoxedExpr(ObjCBoxedExpr *E) {
  OS << "@";
  Visit(E->getSubExpr());
}

void StatInfInstrStmtPrinter::VisitObjCArrayLiteral(ObjCArrayLiteral *E) {
  OS << "@[ ";
  ObjCArrayLiteral::child_range Ch = E->children();
  for (auto I = Ch.begin(), E = Ch.end(); I != E; ++I) {
    if (I != Ch.begin())
      OS << ", ";
    Visit(*I);
  }
  OS << " ]";
}

void StatInfInstrStmtPrinter::VisitObjCDictionaryLiteral(ObjCDictionaryLiteral *E) {
  OS << "@{ ";
  for (unsigned I = 0, N = E->getNumElements(); I != N; ++I) {
    if (I > 0)
      OS << ", ";

    ObjCDictionaryElement Element = E->getKeyValueElement(I);
    Visit(Element.Key);
    OS << " : ";
    Visit(Element.Value);
    if (Element.isPackExpansion())
      OS << "...";
  }
  OS << " }";
}

void StatInfInstrStmtPrinter::VisitObjCEncodeExpr(ObjCEncodeExpr *Node) {
  OS << "@encode(";
  Node->getEncodedType().print(OS, Policy);
  OS << ')';
}

void StatInfInstrStmtPrinter::VisitObjCSelectorExpr(ObjCSelectorExpr *Node) {
  OS << "@selector(";
  Node->getSelector().print(OS);
  OS << ')';
}

void StatInfInstrStmtPrinter::VisitObjCProtocolExpr(ObjCProtocolExpr *Node) {
  OS << "@protocol(" << *Node->getProtocol() << ')';
}

void StatInfInstrStmtPrinter::VisitObjCMessageExpr(ObjCMessageExpr *Mess) {
  OS << "[";
  switch (Mess->getReceiverKind()) {
  case ObjCMessageExpr::Instance:
    PrintExpr(Mess->getInstanceReceiver());
    break;

  case ObjCMessageExpr::Class:
    Mess->getClassReceiver().print(OS, Policy);
    break;

  case ObjCMessageExpr::SuperInstance:
  case ObjCMessageExpr::SuperClass:
    OS << "Super";
    break;
  }

  OS << ' ';
  Selector selector = Mess->getSelector();
  if (selector.isUnarySelector()) {
    OS << selector.getNameForSlot(0);
  } else {
    for (unsigned i = 0, e = Mess->getNumArgs(); i != e; ++i) {
      if (i < selector.getNumArgs()) {
        if (i > 0) OS << ' ';
        if (selector.getIdentifierInfoForSlot(i))
          OS << selector.getIdentifierInfoForSlot(i)->getName() << ':';
        else
           OS << ":";
      }
      else OS << ", "; // Handle variadic methods.

      PrintExpr(Mess->getArg(i));
    }
  }
  OS << "]";
}

void StatInfInstrStmtPrinter::VisitObjCBoolLiteralExpr(ObjCBoolLiteralExpr *Node) {
  OS << (Node->getValue() ? "__objc_yes" : "__objc_no");
}

void
StatInfInstrStmtPrinter::VisitObjCIndirectCopyRestoreExpr(ObjCIndirectCopyRestoreExpr *E) {
  PrintExpr(E->getSubExpr());
}

void
StatInfInstrStmtPrinter::VisitObjCBridgedCastExpr(ObjCBridgedCastExpr *E) {
  OS << '(' << E->getBridgeKindName();
  E->getType().print(OS, Policy);
  OS << ')';
  PrintExpr(E->getSubExpr());
}

void StatInfInstrStmtPrinter::VisitBlockExpr(BlockExpr *Node) {
  BlockDecl *BD = Node->getBlockDecl();
  OS << "^";

  const FunctionType *AFT = Node->getFunctionType();

  if (isa<FunctionNoProtoType>(AFT)) {
    OS << "()";
  } else if (!BD->param_empty() || cast<FunctionProtoType>(AFT)->isVariadic()) {
    OS << '(';
    for (BlockDecl::param_iterator AI = BD->param_begin(),
         E = BD->param_end(); AI != E; ++AI) {
      if (AI != BD->param_begin()) OS << ", ";
      std::string ParamStr = (*AI)->getNameAsString();
      (*AI)->getType().print(OS, Policy, ParamStr);
    }

    const auto *FT = cast<FunctionProtoType>(AFT);
    if (FT->isVariadic()) {
      if (!BD->param_empty()) OS << ", ";
      OS << "...";
    }
    OS << ')';
  }
  OS << "{ }";
}

void StatInfInstrStmtPrinter::VisitOpaqueValueExpr(OpaqueValueExpr *Node) {
  PrintExpr(Node->getSourceExpr());
}

void StatInfInstrStmtPrinter::VisitTypoExpr(TypoExpr *Node) {
  // TODO: Print something reasonable for a TypoExpr, if necessary.
  llvm_unreachable("Cannot print TypoExpr nodes");
}

void StatInfInstrStmtPrinter::VisitRecoveryExpr(RecoveryExpr *Node) {
  OS << "<recovery-expr>(";
  const char *Sep = "";
  for (Expr *E : Node->subExpressions()) {
    OS << Sep;
    PrintExpr(E);
    Sep = ", ";
  }
  OS << ')';
}

void StatInfInstrStmtPrinter::VisitAsTypeExpr(AsTypeExpr *Node) {
  OS << "__builtin_astype(";
  PrintExpr(Node->getSrcExpr());
  OS << ", ";
  Node->getType().print(OS, Policy);
  OS << ")";
}
} // namespace