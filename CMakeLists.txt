cmake_minimum_required(VERSION 3.0)
enable_language(C)
project(elibgit2)

set(BUILD_SHARED_LIBS OFF)
set(POSITION_INDEPENDENT_CODE ON)

add_subdirectory(libgit2)

find_library(git2 libgit2.a)
add_subdirectory(src)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/Cask DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/test DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
