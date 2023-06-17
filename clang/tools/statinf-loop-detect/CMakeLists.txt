set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Option
  Support
  )

add_clang_tool(statinf-loop-detect
  StatInfLoopDetect.cpp
  )

clang_target_link_libraries(statinf-loop-detect
  PRIVATE
  clangAST
  clangBasic
  clangDriver
  clangFrontend
  clangTooling
  clangToolingSyntax
  libstatinfclang
  )
