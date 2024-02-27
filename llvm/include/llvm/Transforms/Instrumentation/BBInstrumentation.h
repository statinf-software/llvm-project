
#ifndef LLVM_TRANSFORMS_INSTRUMENTATION_BBINSTRUMENTATION_H
#define LLVM_TRANSFORMS_INSTRUMENTATION_BBINSTRUMENTATION_H

#include "llvm/IR/PassManager.h"

namespace llvm {

class Function;

struct BBInstrumentationPass
    : public PassInfoMixin<BBInstrumentationPass> {
  BBInstrumentationPass() : PostInlining(true) {}
  BBInstrumentationPass(bool PostInlining) : PostInlining(PostInlining) {}

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);

  void printPipeline(raw_ostream &OS,
                     function_ref<StringRef(StringRef)> MapClassName2PassName);

  bool PostInlining;

  static bool isRequired() { return true; }
};

} // namespace llvm

#endif // LLVM_TRANSFORMS_INSTRUMENTATION_BBINSTRUMENTATION_H
