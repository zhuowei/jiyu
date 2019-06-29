cmake_minimum_required(VERSION 2.6)
project(jiyu)
find_package (Threads)

set(LLVM_DIR "${PROJECT_SOURCE_DIR}/llvm/lib/cmake/llvm/")
find_package(LLVM REQUIRED CONFIG)

if(NOT CMAKE_SIZEOF_VOID_P EQUAL 8)
    add_definitions(-DENV32)
else()
    add_definitions(-DENV64)
endif()

#include_directories()

set (JIYU_SRCS
    src/main.cpp
    src/parser.cpp
    src/lexer.cpp
    src/compiler.cpp
    src/llvm.cpp
    src/sema.cpp
    src/copier.cpp
)


if (UNIX AND NOT APPLE)
    set(LINUX TRUE)
endif ()

# compiler options
if (WIN32)
else()
	add_definitions(-Wno-writable-strings --std=c++11 -g -fno-rtti -Wall -Wextra -pedantic -Wno-unused-parameter)
endif()

if (APPLE)
    add_definitions(-DMACOSX)
endif()

if (LINUX)
    add_definitions(-DLINUX)
endif ()


include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_TARGETS_TO_BUILD} asmparser asmprinter passes orcjit)
add_executable(jiyu ${JIYU_SRCS})
target_link_libraries(jiyu ${CMAKE_THREAD_LIBS_INIT} ${LLVM_LIBS})
