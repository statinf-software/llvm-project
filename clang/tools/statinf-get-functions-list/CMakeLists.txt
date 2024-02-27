set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Option
  Support
  FrontendOpenMP
  )

add_clang_tool(statinf-get-functions-list
  StatInfGetFuncList.cpp
  )

clang_target_link_libraries(statinf-get-functions-list
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
