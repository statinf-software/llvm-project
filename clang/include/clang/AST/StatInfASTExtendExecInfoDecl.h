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
#include "llvm/ADT/SmallSet.h"

#include <vector>
#include <exception>

namespace clang {
  class StatInfASTExtendExecInfoDecl : public DeclVisitor<StatInfASTExtendExecInfoDecl> {
public:
    class Bitstream {
    public:
        enum Endianness {
            E_BIG_ENDIAN,
            E_LITTLE_ENDIAN
        };
    private:
        std::vector<uint8_t> bitstream_trace;
        size_t trace_size = 0; //number of Bytes in a trace
        size_t trace_char_idx = 0; // index of the current char in the vector
        uint8_t trace_bit_idx = 7; // index of the current bit in the current char, always between [7,0], decremented to read from left to right
        size_t eot_char_idx; //char index marking the end of the trace

        uint16_t word_count_in_bitstream;
        uint16_t bit_count_remaining;
        size_t timestamp_idx;
        size_t eot_timestamp_idx;
        uint16_t word_count_timestamp;
        uint16_t trace_count;

        Endianness trace_endianness;
        Endianness host_endianness;
    public:
        Bitstream(const std::string & bitstream_filename, size_t trace_size, Endianness en=Endianness::E_BIG_ENDIAN);

        // A bitstream dump can contain multiple traces
        // each with its header
        bool loadNewTrace();

        bool loadHeader();

        bool getTraceBit(uint8_t *ret);
        // Consume n bits from the bitstream, no size restriction
        void consumeNbits(uint8_t n);

        //End of trace
        bool EOPayload();
        //End of timestamp
        bool EOTimestamp();

        bool EOFile();

        size_t getTimestamp();
    };

private:
    const ASTContext &Context;
    Bitstream *_bitstream;
    //bool contains_structural_analysis_data = false;
    //bool contains_temporal_analysis_data = false;
    StringRef EntryPointName;

    llvm::SmallSet<std::string,8> missing_func_body;

  public:
    StatInfASTExtendExecInfoDecl(const ASTContext &Context, StringRef en, Bitstream *bs/*, bool structural, bool temporal*/)
        : Context(Context), _bitstream(bs), 
        /*contains_structural_analysis_data(structural), contains_temporal_analysis_data(temporal), */
        EntryPointName(en) {}

    llvm::SmallSet<std::string,8> getMissingFunctionBody() {return missing_func_body; }

    Bitstream *bitstream() { return _bitstream; }

    void VisitFunctionDecl(FunctionDecl *F);
    void VisitVarDecl(VarDecl *D);
    void VisitParmVarDecl(ParmVarDecl *D);
  };
}
