set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Option
  Support
  )

add_clang_tool(statinf-instrumentation
  StatInfInstrumentation.cpp
  )

clang_target_link_libraries(statinf-instrumentation
  PRIVATE
  clangAST
  clangBasic
  clangDriver
  clangFrontend
  clangTooling
  clangToolingSyntax
  clangAnalysis
  )
