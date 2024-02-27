set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Option
  Support
  FrontendOpenMP
  )

add_clang_tool(statinf-CFG-with-trace
  StatinfCFGExtendedExecInfo.cpp
  DotCFGPrinter.cpp
  JsonCFGPrinter.cpp
  )

clang_target_link_libraries(statinf-CFG-with-trace
  PRIVATE
  clangAnalysis
  clangAST
  clangASTMatchers
  clangBasic
  clangDriver
  clangFrontend
  clangSema
  clangTooling
  clangToolingSyntax
  clangSerialization
  libstatinfclang
  )
