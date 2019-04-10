cmake_minimum_required(VERSION 2.6)
project(htn)
find_package (Threads)



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
)

if (WIN32)

else()
	add_definitions(-Wno-writable-strings --std=c++11 -g -fno-rtti -Wall -Wextra -pedantic)
endif()

add_executable(htn ${HTN_SRCS})
target_link_libraries(htn ${CMAKE_THREAD_LIBS_INIT})
