cmake_minimum_required(VERSION 2.6)
project(htn)
find_package (Threads)

set(LLVM_DIR "${PROJECT_SOURCE_DIR}/llvm/lib/cmake/llvm/")
find_package(LLVM REQUIRED CONFIG)

if(NOT CMAKE_SIZEOF_VOID_P EQUAL 8)
    add_definitions(-DENV32)
else()
    add_definitions(-DENV64)
endif()

#include_directories()

set (HTN_SRCS
    src/main.cpp
    src/parser.cpp
    src/lexer.cpp
    src/compiler.cpp
    src/llvm.cpp
)

if (WIN32)

else()
	add_definitions(-Wno-writable-strings --std=c++11 -g -fno-rtti -Wall -Wextra -pedantic -Wno-unused-parameter)
endif()

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(LLVM_LIBS X86 asmparser asmprinter passes)
add_executable(htn ${HTN_SRCS})
target_link_libraries(htn ${CMAKE_THREAD_LIBS_INIT} ${LLVM_LIBS})
