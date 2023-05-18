set(SOURCES
 StatInfDiagConsumer.cpp
 StatInfUtils.cpp
)

if(LLVM_ENABLE_PIC OR (WIN32 AND NOT LIBCLANG_BUILD_STATIC))
  set(ENABLE_SHARED SHARED)
endif()

if(NOT LLVM_ENABLE_PIC OR LIBCLANG_BUILD_STATIC)
  set(ENABLE_STATIC STATIC)
endif()

if (MSVC AND ENABLE_SHARED AND ENABLE_STATIC)
  unset(ENABLE_STATIC)
endif()

if(MSVC)
  set(output_name "libstatinfclang")
else()
  set(output_name "statinfclang")
endif()

set(LIBS
  clangAnalysis
  clangAST
  clangBasic
  clangDriver
  clangFrontend
  clangSema
  clangTooling
  clangToolingSyntax
)

add_clang_library(libstatinfclang ${ENABLE_SHARED} ${ENABLE_STATIC} INSTALL_WITH_TOOLCHAIN
  OUTPUT_NAME ${output_name}
  ${SOURCES}

  DEPENDS
  ClangDriverOptions

  LINK_LIBS
  ${LIBS}

  LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Core
  Support
  )