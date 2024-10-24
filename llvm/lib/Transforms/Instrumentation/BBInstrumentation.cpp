#include "llvm/Transforms/Utils/EntryExitInstrumenter.h"
#include "llvm/Transforms/Instrumentation/BBInstrumentation.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils.h"
#include <cxxabi.h>

using namespace llvm;
using namespace std;

/*
PreservedAnalyses BBInstrumentationPass::run(Module &M, ModuleAnalysisManager &AM) {
    llvm::errs() << "MODULE " << M.getName() << "\n";
    for(Function &F : M.getFunctionList()) {
        llvm::errs() << "\t>" << F.getName() << "\n";
    }
    return PreservedAnalyses::all();
}
PreservedAnalyses BBInstrumentationPass::run(Function &F, FunctionAnalysisManager &FAM) {
    LLVMContext &context = F.getContext();
    auto module = F.getParent();
    FunctionType *printType = FunctionType::get(Type::getInt32Ty(context), {Type::getInt8PtrTy(context)}, true);
    FunctionCallee printFunction = module->getOrInsertFunction("printf", printType);

    string functionName = F.getName().str();
    string functionCallVarName = functionName + "_callCount";
    GlobalVariable *functionCallCount = module->getGlobalVariable(functionName + functionCallVarName);
    if(!functionCallCount) {
        functionCallCount = new GlobalVariable(*module, Type::getInt32Ty(context), false, GlobalValue::CommonLinkage, 0, functionCallVarName);
        functionCallCount->setInitializer(ConstantInt::get(Type::getInt32Ty(context), 0));
    }
llvm::errs() << "--------> " << functionName << "\n";
    Instruction *firstInstruction = &F.front().front();
    IRBuilder<> builder(firstInstruction);

    Value *loadedCallCount = builder.CreateLoad(Type::getInt32Ty(context), functionCallCount);
    Value *addedCallCount = builder.CreateAdd(loadedCallCount, builder.getInt32(1));
    builder.CreateStore(addedCallCount, functionCallCount);

    string printLog = functionName + "%d\n";
    Value *funcNamePtr = builder.CreateGlobalStringPtr(printLog);
    builder.CreateCall(printFunction, {funcNamePtr, addedCallCount});

    return PreservedAnalyses::all();
}
*/

static string demangle(const string & str) {
    int status;
    string demangled = str;
    char *realname = abi::__cxa_demangle(str.c_str(), 0, 0, &status);
    if(status == 0) {
        demangled = realname;
    }
    if(realname != NULL)
        free(realname);

    return demangled;
}

static void insertCall(Function &CurFn, StringRef Fun, Instruction *InsertionPt, DebugLoc DL) {
  Module &M = *InsertionPt->getParent()->getParent()->getParent();
  LLVMContext &C = InsertionPt->getParent()->getContext();

    Type *ArgTypes[] = {Type::getInt8PtrTy(C)};

    FunctionCallee Fn = M.getOrInsertFunction(
        Fun, FunctionType::get(Type::getVoidTy(C), ArgTypes, false));

    Value *Args[] = {ConstantExpr::getBitCast(&CurFn, Type::getInt8PtrTy(C))};

    CallInst *Call =
        CallInst::Create(Fn, ArrayRef<Value *>(Args), "", InsertionPt);
    Call->setDebugLoc(DL);
    return;
}

static bool runOnModule(Module &M, bool PostInlining) {
    // llvm::errs() << "MODULE " << M.getName() << "\n";
    // for(Function &F : M.getFunctionList()) {
    //     llvm::errs() << "\t>" << demangle(F.getName().str()) << "\n";
    //     SmallVector<std::pair<unsigned, MDNode *>, 4> MDs;
    //     F.getAllMetadata(MDs);
    //     for (auto &MD : MDs) {
    //       if (MDNode *N = MD.second) {
    //         if (auto *subProgram = dyn_cast<DISubprogram>(N)) {
    //         }
    //       }
    //     }
    // }
    return true;
}

static bool runOnFunction(Function &F, bool PostInlining) {
  bool Changed = false;
  // StringRef EntryAttr = "instrument-function-bbs";
  // if(!F.hasFnAttribute(EntryAttr))
  //   return Changed;

  // // If the attribute is specified, insert instrumentation and then "consume"
  // // the attribute so that it's not inserted again if the pass should happen to
  // // run later for some reason.
  // F.removeFnAttr(EntryAttr);

  // SmallVector<std::pair<unsigned, MDNode *>, 4> MDs;
  // F.getAllMetadata(MDs);
  // bool localChecked = false;
  // for (auto &MD : MDs) {
  //   if (MDNode *N = MD.second) {
  //     if (auto *subProgram = dyn_cast<DISubprogram>(N)) {
  //       localChecked = true;
  //       if(!subProgram->isLocalToUnit() || !subProgram->isDefinition() || subProgram->isMainSubprogram()) {
  //         return Changed;
  //       }
  //     }
  //   }
  // }
  // // if(!localChecked)
  //   // return Changed;

  // BasicBlock &BB = F.getEntryBlock();
  // Instruction &I = *BB.getFirstInsertionPt();

  // string name = demangle(F.getName().str());
  // DebugLoc DL;
  // if (DebugLoc IDL = I.getDebugLoc())
  //     DL = IDL;
  // else if (auto SP = F.getSubprogram())
  //   DL = DILocation::get(SP->getContext(), SP->getScopeLine(), 0, SP);
  
  // insertCall(F, "profile_enter_func", &I, DL);

  // // for (BasicBlock &BB : F) {
  // //   Instruction &I = *BB.getFirstInsertionPt();
  // //   DebugLoc DL;
  // //   if (DebugLoc IDL = I.getDebugLoc())
  // //       DL = IDL;
  // //   else if (auto SP = F.getSubprogram())
  // //     DL = DILocation::get(SP->getContext(), SP->getScopeLine(), 0, SP);
    
  // //   insertCall(F, "profile_enter_bb", &I, DL);

  // //   Changed = true;
  // // }

  return Changed;
}

PreservedAnalyses
llvm::BBInstrumentationPass::run(Module &M, ModuleAnalysisManager &AM) {
  runOnModule(M, PostInlining);
  PreservedAnalyses PA;
  PA.preserveSet<CFGAnalyses>();
  return PA;
}

PreservedAnalyses
llvm::BBInstrumentationPass::run(Function &F, FunctionAnalysisManager &AM) {
  runOnFunction(F, PostInlining);
  PreservedAnalyses PA;
  PA.preserveSet<CFGAnalyses>();
  return PA;
}

void llvm::BBInstrumentationPass::printPipeline(
    raw_ostream &OS, function_ref<StringRef(StringRef)> MapClassName2PassName) {
  static_cast<PassInfoMixin<llvm::BBInstrumentationPass> *>(this)
      ->printPipeline(OS, MapClassName2PassName);
  OS << "<";
  if (PostInlining)
    OS << "post-inline";
  OS << ">";
}
