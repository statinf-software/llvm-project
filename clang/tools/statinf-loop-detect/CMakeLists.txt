set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Option
  Support
  FrontendOpenMP
  )

add_clang_tool(statinf-loop-detect
  StatInfLoopDetect.cpp
  )

clang_target_link_libraries(statinf-loop-detect
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
