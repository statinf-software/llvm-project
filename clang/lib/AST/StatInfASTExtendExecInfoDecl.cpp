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

// void StatInfASTExtendExecInfoDecl::VisitDeclContext(DeclContext *DC, bool Indent) {
//   if (Policy.TerseOutput)
//     return;

//   if (Indent)
//     Indentation += Policy.Indentation;

//   SmallVector<Decl*, 2> Decls;
//   for (DeclContext::decl_iterator D = DC->decls_begin(), DEnd = DC->decls_end();
//        D != DEnd; ++D) {

//     // Don't print ObjCIvarDecls, as they are printed when visiting the
//     // containing ObjCInterfaceDecl.
//     if (isa<ObjCIvarDecl>(*D))
//       continue;

//     // Skip over implicit declarations in pretty-printing mode.
//     if (D->isImplicit())
//       continue;

//     // Don't print implicit specializations, as they are printed when visiting
//     // corresponding templates.
//     if (auto FD = dyn_cast<FunctionDecl>(*D))
//       if (FD->getTemplateSpecializationKind() == TSK_ImplicitInstantiation &&
//           !isa<ClassTemplateSpecializationDecl>(DC))
//         continue;

//     // The next bits of code handle stuff like "struct {int x;} a,b"; we're
//     // forced to merge the declarations because there's no other way to
//     // refer to the struct in question.  When that struct is named instead, we
//     // also need to merge to avoid splitting off a stand-alone struct
//     // declaration that produces the warning ext_no_declarators in some
//     // contexts.
//     //
//     // This limited merging is safe without a bunch of other checks because it
//     // only merges declarations directly referring to the tag, not typedefs.
//     //
//     // Check whether the current declaration should be grouped with a previous
//     // non-free-standing tag declaration.
//     QualType CurDeclType = getDeclType(*D);
//     if (!Decls.empty() && !CurDeclType.isNull()) {
//       QualType BaseType = GetBaseType(CurDeclType);
//       if (!BaseType.isNull() && isa<ElaboratedType>(BaseType) &&
//           cast<ElaboratedType>(BaseType)->getOwnedTagDecl() == Decls[0]) {
//         Decls.push_back(*D);
//         continue;
//       }
//     }

//     // If we have a merged group waiting to be handled, handle it now.
//     if (!Decls.empty())
//       ProcessDeclGroup(Decls);

//     // If the current declaration is not a free standing declaration, save it
//     // so we can merge it with the subsequent declaration(s) using it.
//     if (isa<TagDecl>(*D) && !cast<TagDecl>(*D)->isFreeStanding()) {
//       Decls.push_back(*D);
//       continue;
//     }

//     if (isa<AccessSpecDecl>(*D)) {
//       Indentation -= Policy.Indentation;
//       this->Indent();
//       Print(D->getAccess());
//       Out << ":\n";
//       Indentation += Policy.Indentation;
//       continue;
//     }

//     this->Indent();
//     Visit(*D);

//     // FIXME: Need to be able to tell the StatInfASTExtendExecInfoDecl when
//     const char *Terminator = nullptr;
//     if (isa<OMPThreadPrivateDecl>(*D) || isa<OMPDeclareReductionDecl>(*D) ||
//         isa<OMPDeclareMapperDecl>(*D) || isa<OMPRequiresDecl>(*D) ||
//         isa<OMPAllocateDecl>(*D))
//       Terminator = nullptr;
//     else if (isa<ObjCMethodDecl>(*D) && cast<ObjCMethodDecl>(*D)->hasBody())
//       Terminator = nullptr;
//     else if (auto FD = dyn_cast<FunctionDecl>(*D)) {
//       if (FD->isThisDeclarationADefinition())
//         Terminator = nullptr;
//       else
//         Terminator = ";";
//     } else if (auto TD = dyn_cast<FunctionTemplateDecl>(*D)) {
//       if (TD->getTemplatedDecl()->isThisDeclarationADefinition())
//         Terminator = nullptr;
//       else
//         Terminator = ";";
//     } else if (isa<NamespaceDecl, LinkageSpecDecl, ObjCImplementationDecl,
//                    ObjCInterfaceDecl, ObjCProtocolDecl, ObjCCategoryImplDecl,
//                    ObjCCategoryDecl, HLSLBufferDecl>(*D))
//       Terminator = nullptr;
//     else if (isa<EnumConstantDecl>(*D)) {
//       DeclContext::decl_iterator Next = D;
//       ++Next;
//       if (Next != DEnd)
//         Terminator = ",";
//     } else
//       Terminator = ";";

//     if (Terminator)
//       Out << Terminator;
//     if (!Policy.TerseOutput &&
//         ((isa<FunctionDecl>(*D) &&
//           cast<FunctionDecl>(*D)->doesThisDeclarationHaveABody()) ||
//          (isa<FunctionTemplateDecl>(*D) &&
//           cast<FunctionTemplateDecl>(*D)->getTemplatedDecl()->doesThisDeclarationHaveABody())))
//       ; // StmtPrinter already added '\n' after CompoundStmt.
//     else
//       Out << "\n";

//     // Declare target attribute is special one, natural spelling for the pragma
//     // assumes "ending" construct so print it here.
//     if (D->hasAttr<OMPDeclareTargetDeclAttr>())
//       Out << "#pragma omp end declare target\n";
//   }

//   if (!Decls.empty())
//     ProcessDeclGroup(Decls);

//   if (Indent)
//     Indentation -= Policy.Indentation;
// }

void StatInfASTExtendExecInfoDecl::VisitFunctionDecl(FunctionDecl *F) {
    if (!F->getBody()) {
        missing_func_body.insert(F->getName().str());
        return;
    }

    if(contains_temporal_analysis_data) {
        size_t time;
        if(!getNbits(time_num_bits, &time))
            return;
        // std::cout << "-> " << F->getName().str() << ": " << std::hex << time << std::endl;
        F->addEntryTimestamp(time);
    }

    if(contains_structural_analysis_data) {
        if(F->getBody()) {
            // std::cout << "-> Enter " << F->getName().str() << "\n";
            StatInfASTExtendExecInfoStmt stmtVisitor(this, contains_structural_analysis_data, contains_temporal_analysis_data);

            stmtVisitor.Visit(F->getBody());
            // std::cout << "-> Exit " << F->getName().str() << "\n";
        }
        else
            missing_func_body.insert(F->getName().str());
    }

    if(contains_temporal_analysis_data) {
        size_t time;
        if(!getNbits(time_num_bits, &time))
            return;
        // std::cout << "-> " << F->getName().str() << ": " << std::hex << time << std::endl;
        F->addExitTimestamp(time);
    }
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