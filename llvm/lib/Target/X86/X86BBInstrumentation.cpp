//==- X86BBInstrumentation.cpp - Replace rets with thunks or inline thunks --=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
///
/// Pass that replaces ret instructions with a jmp to __x86_return_thunk.
///
/// This corresponds to -mfunction-return=thunk-extern or
/// __attribute__((function_return("thunk-extern").
///
/// This pass is a minimal implementation necessary to help mitigate
/// RetBleed for the Linux kernel.
///
/// Should support for thunk or thunk-inline be necessary in the future, then
/// this pass should be combined with x86-retpoline-thunks which already has
/// machinery to emit thunks. Until then, YAGNI.
///
/// This pass is very similar to x86-lvi-ret.
///
//===----------------------------------------------------------------------===//

#include "X86.h"
#include "X86InstrInfo.h"
#include "X86Subtarget.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Support/Debug.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Module.h"
#include "llvm/CodeGen/GlobalISel/CallLowering.h"

using namespace llvm;

#define PASS_KEY "x86-bb-instrumentation"
#define DEBUG_TYPE PASS_KEY

struct X86BBInstrumentation final : public MachineFunctionPass {
  static char ID;
  X86BBInstrumentation() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override { return "X86 BB Instrumentation"; }
  bool runOnMachineFunction(MachineFunction &MF) override;
};

char X86BBInstrumentation::ID = 0;

bool X86BBInstrumentation::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << getPassName() << "\n");

  bool Modified = false;

  Function &F = MF.getFunction();

  StringRef EntryAttr = "instrument-function-bbs";
  if(!F.hasFnAttribute(EntryAttr))
    return Modified;
  F.removeFnAttr(EntryAttr);

  llvm::errs() << "X86 Instrument " << F.getName() << "\n";
  const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();

  MCContext &Ctx = MF.getContext();
  SmallString<32> MangledName;
  DebugLoc DL;
  Mangler::getNameWithPrefix(MangledName, Twine("profile_enter_bb"), MF.getDataLayout());
  MCSymbol *Symbol = Ctx.getOrCreateSymbol(MangledName);

  FunctionCallee Fn = F.getParent()->getOrInsertFunction(
      "profile_enter_bb", FunctionType::get(Type::getVoidTy(F.getContext()), false));

  const GlobalValue *GV = dyn_cast<GlobalValue>(Fn.getCallee());
  if(GV == nullptr) {
    return Modified;
  }

  const X86Subtarget *Subtarget = &MF.getSubtarget<X86Subtarget>();
  unsigned char OpFlags = Subtarget->classifyGlobalFunctionReference(GV);

  bool NeedLoad = OpFlags == X86II::MO_DLLIMPORT ||
                    OpFlags == X86II::MO_GOTPCREL ||
                    OpFlags == X86II::MO_GOTPCREL_NORELAX ||
                    OpFlags == X86II::MO_COFFSTUB;
  unsigned CallOpc = NeedLoad
                          ? (Subtarget->is64Bit() ? X86::CALL64m : X86::CALL32m)
                          : (Subtarget->is64Bit() ? X86::CALL64pcrel32 : X86::CALLpcrel32);

  for (auto &MBB : MF) {
    MachineBasicBlock::iterator MBBI = MBB.begin();
    DebugLoc DL;
    MBB.setLabelMustBeEmitted();

    BuildMI(MBB, MBBI, DL, TII->get(X86::PUSH64r))
      .addReg(X86::RDI)
    ;
    //This 1st mov needs to be transformed into
    //call next
    //next:
    //pop RAX
    //mov RDI,RAX
    BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64rr))
      .addReg(X86::RDI)
      .addReg(X86::RIP)
      .addUse(X86::RIP)
    ;
    MachineInstrBuilder MIB = BuildMI(MBB, MBBI, DL, TII->get(CallOpc));
    if (NeedLoad)
      MIB.addReg(Subtarget->is64Bit() ? X86::RIP : 0).addImm(1).addReg(0);
    MIB.addSym(Symbol, OpFlags);
    if (NeedLoad)
      MIB.addReg(0);

    BuildMI(MBB, MBBI, DL, TII->get(X86::POP64r))
      .addReg(X86::RDI)
    ;

    Modified = true;
  }

  return Modified;
}

INITIALIZE_PASS(X86BBInstrumentation, PASS_KEY, "X86 BB Instrumentation", false, false)

FunctionPass *llvm::createX86BBInstrumentationPass() {
  return new X86BBInstrumentation();
}
