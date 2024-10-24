//==- AArch64BBInstrumentation.cpp - Replace rets with thunks or inline thunks --=//
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

#include "AArch64.h"
#include "AArch64InstrInfo.h"
#include "AArch64Subtarget.h"
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

#define PASS_KEY "aarch64-bb-instrumentation"
#define DEBUG_TYPE PASS_KEY

struct AArch64BBInstrumentation final : public MachineFunctionPass {
  static char ID;
  AArch64BBInstrumentation() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override { return "AArch64 BB Instrumentation"; }
  bool runOnMachineFunction(MachineFunction &MF) override;
};

char AArch64BBInstrumentation::ID = 0;

static void insertProfilingCode(const TargetInstrInfo *TII, MCSymbol *FunProfSymb, MachineBasicBlock &MBB, MachineBasicBlock::iterator &MBBI) {
  DebugLoc DL;
  /*
  stp     x0, x30, [sp, #-16]!
  //bl      4
  //mov     x0, x30
  bl      profile_enter_bb
  ldp     x0, x30, [sp], #16
  */

  BuildMI(MBB, MBBI, DL, TII->get(AArch64::STPXpre))
    .addDef(AArch64::SP)
    .addReg(AArch64::X0)
    .addReg(AArch64::LR) //X30
    .addReg(AArch64::SP)
    .addImm(-2) //*8
  ;
  // BuildMI(MBB, MBBI, DL, TII->get(AArch64::BL))
  //   .addImm(1) //*4 -> 32bits instruction
  // ;
  // BuildMI(MBB, MBBI, DL, TII->get(AArch64::ADDXri))
  //   .addReg(AArch64::X0)
  //   .addReg(AArch64::LR) //x30
  //   .addImm(0)
  //   .addImm(0)
  // ;
  BuildMI(MBB, MBBI, DL, TII->get(AArch64::BL))
    .addSym(FunProfSymb)
  ;
  BuildMI(MBB, MBBI, DL, TII->get(AArch64::LDPXpost))
    .addDef(AArch64::SP)
    .addReg(AArch64::X0)
    .addReg(AArch64::LR) //x30
    .addReg(AArch64::SP)
    .addImm(2)//*8
  ;
}

bool AArch64BBInstrumentation::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << getPassName() << "\n");

  bool Modified = false;

  Function &F = MF.getFunction();

  StringRef EntryAttr = "instrument-function-bbs";
  if(!F.hasFnAttribute(EntryAttr))
    return Modified;
  F.removeFnAttr(EntryAttr);

  llvm::errs() << "AArch64 Instrument " << F.getName() << "\n";
  const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();

  MCContext &Ctx = MF.getContext();
  SmallString<32> MangledName;
  DebugLoc DL;
  Mangler::getNameWithPrefix(MangledName, Twine("profile_enter_bb"), MF.getDataLayout());
  MCSymbol *FunProfSymb = Ctx.getOrCreateSymbol(MangledName);

  for (auto &MBB : MF) {
    MachineBasicBlock::iterator MBBI = MBB.begin();

    //In LLVM Calls are not considered as the end of a BB
    // as it's assumed that the call will return and so
    // do not break the flow of the BB
    for(auto MBBI=MBB.begin(), MBBE=MBB.end() ; MBBI != MBBE ; MBBI++) {
      auto &MI = *MBBI;
      if(MI.isCall()) {
        MBBI++;
        insertProfilingCode(TII, FunProfSymb, MBB, MBBI);
        MBBI--;
      }
    }

    insertProfilingCode(TII, FunProfSymb, MBB, MBBI);

    Modified = true;
  }

  return Modified;
}

INITIALIZE_PASS(AArch64BBInstrumentation, PASS_KEY, "AArch64 BB Instrumentation", false, false)

FunctionPass *llvm::createAArch64BBInstrumentationPass() {
  return new AArch64BBInstrumentation();
}


/*
adrp    x0, .LBB0_0             |_ load address of basic block
    add     x0, x0, :lo12:.LBB0_0   |
    call    profile_enter_bb
    
    // BuildMI(MBB, MBBI, DL, TII->get(AArch64::ADRP))
    //   .addReg(AArch64::X0)
    //   .addSym(MBB.getSymbol())
    // ;
    // BuildMI(MBB, MBBI, DL, TII->get(AArch64::ADDXri))
    //   .addReg(AArch64::X0)
    //   .addReg(AArch64::X0)
    //   .addSym(MBB.getSymbol(), AArch64II::MO_NC | AArch64II::MO_PAGEOFF)
    //   .addImm(0)
    // ;
    // BuildMI(MBB, MBBI, DL, TII->get(AArch64::LDRXui))
    //   .addReg(AArch64::X0)
    //   .addReg(AArch64::SP)
    //   .addImm(2)
    // ;
*/