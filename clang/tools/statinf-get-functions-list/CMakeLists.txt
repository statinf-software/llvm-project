set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Option
  Support
  )

add_clang_tool(statinf-get-functions-list
  StatInfGetFuncList.cpp
  )

clang_target_link_libraries(statinf-get-functions-list
  PRIVATE
  clangAST
  clangBasic
  clangDriver
  clangFrontend
  clangTooling
  clangToolingSyntax
  libstatinfclang
  )
