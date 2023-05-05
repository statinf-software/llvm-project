//===--- StatInfASTExtendExecInfoDecl.cpp - Printing implementation for Decl ASTs ----------===//
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
#include "clang/AST/StatInfASTExtendExecInfoDecl.h"
#include "clang/AST/StatInfASTExtendExecInfoStmt.h"

#include <math.h>
#include <iostream>

namespace clang {
//----------------------------------------------------------------------------
// Common C declarations
//----------------------------------------------------------------------------

void StatInfASTExtendExecInfoDecl::VisitFunctionDecl(FunctionDecl *F) {
    if(contains_temporal_analysis_data && EntryPointName == F->getName()) {
        size_t time;
        if(!getNbits(time_num_bits, &time))
            return;
        // std::cout << "-> " << F->getName().str() << ": " << std::hex << time << std::endl;
        F->addEntryTimestamp(time);
    }

    if(contains_structural_analysis_data) {
        if(F->getBody()) {
            // llvm::outs() << "-> Enter " << F->getName().str() << "\n";
            StatInfASTExtendExecInfoStmt stmtVisitor(this, contains_structural_analysis_data, contains_temporal_analysis_data);

            stmtVisitor.Visit(F->getBody());
            // llvm::outs() << "-> Exit " << F->getName().str() << "\n";
        }
        else
            missing_func_body.insert(F->getName().str());
    }

    if(contains_temporal_analysis_data && EntryPointName == F->getName()) {
        size_t time;
        if(!getNbits(time_num_bits, &time))
            return;
        // std::cout << "-> " << F->getName().str() << ": " << std::hex << time << std::endl;
        F->addExitTimestamp(time);
    }
}

void StatInfASTExtendExecInfoDecl::VisitVarDecl(VarDecl *D) {
  if (Expr *Init = D->getInit()) {
    StatInfASTExtendExecInfoStmt stmtVisitor(this, contains_structural_analysis_data, contains_temporal_analysis_data);
    stmtVisitor.Visit(Init);
  }
}

void StatInfASTExtendExecInfoDecl::VisitParmVarDecl(ParmVarDecl *D) {
  VisitVarDecl(D);
}


bool StatInfASTExtendExecInfoDecl::getSmallNBits(uint8_t n, uint8_t *ret) {
    if(n > 8) {
        llvm::errs() << "Error: can't get more than 8 bits at a time -- " << (int)n << "\n";
        return false;
    }

    if(EOBS()) {
        llvm::errs() << "Error: End of Bitstream reached\n";
        return false;
    }

    *ret = 0;
    uint8_t msb = bitstream_trace[trace_char_idx] & (0xff >> (7-trace_bit_idx));
    uint8_t lsb = 0;
    if(n <= trace_bit_idx+1) {//+1 as we start at 0
        *ret = msb >> (trace_bit_idx+1-n);
    }
    else {
        if(trace_char_idx+1 > bitstream_trace.size()) {
            llvm::errs() << "Error: missing " << (trace_bit_idx - n) << " bits at the end of the stream\n";
            trace_char_idx = -1;
            return false;
        }
        uint8_t consumed_msb = trace_bit_idx+1;
        uint8_t remaining_to_cons = n-consumed_msb;
        lsb = bitstream_trace[trace_char_idx+1] & (0xff << (8-remaining_to_cons));
        lsb = lsb >> (8-remaining_to_cons);
        *ret = (msb << remaining_to_cons) | lsb;
    }
    
    // std::cout << "======> to read: " << (int)n << ", c idx: " << (int)trace_char_idx << ", b idx: " << (int)trace_bit_idx
    // << " -- c[0]: 0x" << std::hex << (int)bitstream_trace[trace_char_idx] << " -- c[1]: 0x" << std::hex << (int)bitstream_trace[trace_char_idx+1] 
    // << " -- msb: 0x" << std::hex << (int)msb << " -- lsb: 0x" << std::hex << (int)lsb
    // << " ---> ret: " << std::hex << (int)*ret << std::endl;

    consumeNbits(n);
    return true;
}
void StatInfASTExtendExecInfoDecl::consumeNbits(uint8_t n) {
    while(n > 0) {
        if(trace_bit_idx == 0) {
            trace_bit_idx = 7;
            trace_char_idx += 1;
        }
        else
            trace_bit_idx -= 1;
        --n;
    }
}

bool StatInfASTExtendExecInfoDecl::EOBS() {
    return (trace_char_idx >= bitstream_trace.size());
}

}