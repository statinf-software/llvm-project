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
    const ASTContext &Context;
    const std::vector<unsigned char> &bitstream_trace;
    bool contains_structural_analysis_data = false;
    bool contains_temporal_analysis_data = false;

    size_t trace_char_idx = 0; // index of the current char in the vector
    uint8_t trace_bit_idx = 7; // index of the current bit in the current char, always between [7,0], decremented to read from left to right

    size_t time_num_bits; //number of bits to code a timestamp
    StringRef EntryPointName;

    llvm::SmallSet<std::string,8> missing_func_body;

  public:
    StatInfASTExtendExecInfoDecl(const ASTContext &Context, StringRef en, const std::vector<unsigned char> &bt, bool structural, bool temporal, size_t tnb)
        : Context(Context), bitstream_trace(bt), 
        contains_structural_analysis_data(structural), contains_temporal_analysis_data(temporal), 
        time_num_bits(tnb), EntryPointName(en) {}

    llvm::SmallSet<std::string,8> getMissingFunctionBody() {return missing_func_body; }

    bool getSmallNBits(uint8_t n, uint8_t *ret);
    // Consume n bits from the bitstream, no size restriction
    void consumeNbits(uint8_t n);
      
    void VisitFunctionDecl(FunctionDecl *F);

    // void VisitDeclContext(DeclContext *DC, bool Indent = true);

    //true if end of bistream
    bool EOBS();

        // Get n bits from the bitstream, maximum 8 bits at a time
    template<typename Ty>
      bool getNbits(uint8_t n, Ty* valret){
          if(n > sizeof(valret)*8) {
              llvm::errs() << "Error: can't get more than " << (sizeof(valret)*8) << " bits at a time -- " << n << "\n";
              return false;
          }
          uint8_t num_bytes = ceil(n / 8.0f);

          if(trace_char_idx+num_bytes > bitstream_trace.size()) {
              llvm::errs() << trace_char_idx << "+" << num_bytes << ">" << bitstream_trace.size() << "\n";
              llvm::errs() << "Error: Not enough bytes available in the bitstream\n";
              trace_char_idx = -1;
              return false;
          }

          uint8_t cval[num_bytes];
          *valret = 0;
          for(uint8_t i = 0 ; i < num_bytes-1 ; ++i) {
              if(!getSmallNBits(8, &cval[i]))
                  return false;
          }

          uint8_t rest = n - 8 * (num_bytes-1);
          if(rest) {
              if(!getSmallNBits(rest, &cval[num_bytes-1]))
                  return false;
          }

          uint8_t i;
          *valret = cval[0];
          if(num_bytes-1) {
              for(i=1 ; i < num_bytes-1 ; ++i) {
                  *valret = ((*valret) << 8) | cval[i];
              }
              *valret = ((*valret) << rest) | cval[i];
          }
          return true;
      }

  };
}
