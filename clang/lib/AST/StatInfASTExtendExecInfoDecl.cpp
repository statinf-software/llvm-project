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
#include <fstream>
#include <iostream>
#include <arpa/inet.h>
#include <math.h>

namespace clang {
//----------------------------------------------------------------------------
// Common C declarations
//----------------------------------------------------------------------------

void StatInfASTExtendExecInfoDecl::VisitFunctionDecl(FunctionDecl *F) {
    // if(contains_temporal_analysis_data) {
    //     size_t time;
    //     if(!_bitstream->getNbits(_bitstream->getTimeNumBits(), &time))
    //         return;
    //     // std::cout << "-> " << F->getName().str() << ": " << std::hex << time << std::endl;
    //     F->addEntryTimestamp(time);
    // }

    if(_bitstream->EOPayload() && !_bitstream->loadNewTrace()) {
        return;
    }

    // if(contains_structural_analysis_data) {
        if(F->getBody()) {
            // llvm::outs() << "-> Enter " << F->getName().str() << "\n";
            StatInfASTExtendExecInfoStmt stmtVisitor(this/*, contains_structural_analysis_data, contains_temporal_analysis_data*/);

            stmtVisitor.Visit(F->getBody());
            // llvm::outs() << "-> Exit " << F->getName().str() << "\n";
        }
        else
            missing_func_body.insert(F->getName().str());
    // }

    // if(contains_temporal_analysis_data) {
    //     size_t time;
    //     if(!_bitstream->getNbits(_bitstream->getTimeNumBits(), &time))
    //         return;
    //     // std::cout << "-> " << F->getName().str() << ": " << std::hex << time << std::endl;
    //     F->addExitTimestamp(time);
    // }

    if(!_bitstream->EOTimestamp() && EntryPointName == F->getName()) {
        F->addEntryTimestamp(_bitstream->getTimestamp());
        F->addExitTimestamp(_bitstream->getTimestamp());
    }
}

void StatInfASTExtendExecInfoDecl::VisitVarDecl(VarDecl *D) {
  if (Expr *Init = D->getInit()) {
    StatInfASTExtendExecInfoStmt stmtVisitor(this/*, contains_structural_analysis_data, contains_temporal_analysis_data*/);
    stmtVisitor.Visit(Init);
  }
}

void StatInfASTExtendExecInfoDecl::VisitParmVarDecl(ParmVarDecl *D) {
  VisitVarDecl(D);
}


//=======================================================================================
// StatInfASTExtendExecInfoDecl::Bitstream

StatInfASTExtendExecInfoDecl::Bitstream::Bitstream(const std::string &bitstream_file, size_t ts, Endianness en) {
    std::basic_ifstream<char> bitstream_fs(bitstream_file, std::ios::binary | std::ios::in);
    if(!bitstream_fs.is_open()) {
      llvm::errs() << "Error opening " << bitstream_file << "\n";
      return;
    }
    bitstream_trace.insert(bitstream_trace.end(), std::istreambuf_iterator<char>(bitstream_fs), {});
    bitstream_fs.close();

    host_endianness = ( htonl(42) == 42 ) ? Endianness::E_BIG_ENDIAN : Endianness::E_LITTLE_ENDIAN;
    trace_endianness = en;

    trace_size = ts;

    eot_timestamp_idx = 0;
    trace_char_idx = 0;
    trace_count = 0;
    loadHeader();
}

bool StatInfASTExtendExecInfoDecl::Bitstream::loadNewTrace() {
    trace_char_idx = trace_count*trace_size;
    return loadHeader();
}

bool StatInfASTExtendExecInfoDecl::Bitstream::loadHeader() {
    if(EOFile())
        return false;
    
    /*
    bit                      | value    | comment
---------------------------------------------------------
[0..15]                  | XXXX     |  N nombre de mots (16 bits) utilisé pour décrire le bitstream. N entre 0 et 64ko
[16..31]                 | XXXX     | n0 nombre de bit restants après N mots (16 bits). n0 entre 0 et 15
[32..47]                 | XXXX     | N_t nombre de mots (16 bits) utilisé par la section timestamps
[48..N*16+n0-1]          | XXX      | valid payload
[N*16+n0..Last-N_t*16-1] | XXXX     | invalid written decison bits (payload)
[Last-N_t*16..Last-1]    | XXXX     | timestamps
    */
    word_count_in_bitstream = 
        (trace_endianness == Endianness::E_LITTLE_ENDIAN) 
            ? (bitstream_trace[trace_char_idx+1] << 8) | bitstream_trace[trace_char_idx]
            : (bitstream_trace[trace_char_idx] << 8) | bitstream_trace[trace_char_idx+1];

    bit_count_remaining = 
        (trace_endianness == Endianness::E_LITTLE_ENDIAN) 
            ? (bitstream_trace[trace_char_idx+3] << 8) | bitstream_trace[trace_char_idx+2]
            : (bitstream_trace[trace_char_idx+2] << 8) | bitstream_trace[trace_char_idx+3];

    word_count_timestamp = 
        (trace_endianness == Endianness::E_LITTLE_ENDIAN) 
            ? (bitstream_trace[trace_char_idx+5] << 8) | bitstream_trace[trace_char_idx+4]
            : (bitstream_trace[trace_char_idx+4] << 8) | bitstream_trace[trace_char_idx+5];

    if(bit_count_remaining > 0)
        ++word_count_in_bitstream;

    ++trace_count;
    trace_bit_idx = 7;
    trace_char_idx += 6;
    eot_char_idx = trace_char_idx + word_count_in_bitstream*2;
    timestamp_idx = bitstream_trace.size();
    eot_timestamp_idx = timestamp_idx - word_count_timestamp*2;

    if(trace_endianness == Endianness::E_LITTLE_ENDIAN) {
        for(size_t i=trace_char_idx ; i < eot_char_idx ; i+=2) {
            uint8_t tmp = bitstream_trace[i];
            bitstream_trace[i] = bitstream_trace[i+1];
            bitstream_trace[i+1] = tmp;
        }
        if(word_count_timestamp > 0) {
            for(size_t i=timestamp_idx-4 ; i > eot_timestamp_idx ; i-=4) {
                uint8_t tmp = bitstream_trace[i];
                bitstream_trace[i] = bitstream_trace[i+3];
                bitstream_trace[i+3] = tmp;
                tmp = bitstream_trace[i+1];
                bitstream_trace[i] = bitstream_trace[i+2];
                bitstream_trace[i+2] = tmp;
            }
        }
    }

    // llvm::errs() << "-----------------------\n";
    // llvm::errs() << "Start trace\n";
    // llvm::errs() << "Trace num " << trace_count << "\n";
    // llvm::errs() << "Word count " << word_count_in_bitstream << "\n"; 
    // llvm::errs() << "Bit count remaining " << bit_count_remaining << "\n";
    // llvm::errs() << "Trace char idx " << trace_char_idx << "\n";
    // llvm::errs() << "Timestamp word count " << word_count_timestamp << "\n";
    // llvm::errs() << "EO trace " << eot_char_idx << "\n";
    // llvm::errs() << "Timestamp idx " << timestamp_idx << "\n";
    // llvm::errs() << "EO timestamp " << eot_timestamp_idx << "\n";
    // llvm::errs() << "Timestamp count " << ((eot_timestamp_idx-timestamp_idx)/4) << "\n";
    // llvm::errs() << "Bitstream size " << bitstream_trace.size() << "\n";

    return true;
}

bool StatInfASTExtendExecInfoDecl::Bitstream::getTraceBit(uint8_t *ret) {
    if(EOPayload()) {
        llvm::errs() << "Error: End of Trace reached\n";
        return false;
    }

    uint8_t tmp = bitstream_trace[trace_char_idx] & (0x1 << trace_bit_idx);
    *ret = tmp >> trace_bit_idx;
    consumeNbits(1);
    return true;
}
void StatInfASTExtendExecInfoDecl::Bitstream::consumeNbits(uint8_t n) {
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

bool StatInfASTExtendExecInfoDecl::Bitstream::EOPayload() {
    return (trace_char_idx >= eot_char_idx);
}

bool StatInfASTExtendExecInfoDecl::Bitstream::EOTimestamp() {
    return timestamp_idx != eot_timestamp_idx;
}

bool StatInfASTExtendExecInfoDecl::Bitstream::EOFile() {
    return trace_char_idx >= bitstream_trace.size();
}

size_t StatInfASTExtendExecInfoDecl::Bitstream::getTimestamp() {
    uint32_t timestamp = 0;

    if(timestamp_idx-4 < eot_timestamp_idx) {
        llvm::errs() << "Error: Not enough bytes available in the bitstream to get a timestamp\n";
        timestamp_idx = eot_timestamp_idx;
        return false;
    }

    timestamp_idx -= 4;
    for(uint8_t i = 0 ; i < 4 ; ++i) 
        timestamp = timestamp | (bitstream_trace[timestamp_idx+i] << (8*i));

    return timestamp;
}

}