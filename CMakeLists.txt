cmake_minimum_required(VERSION 3.0)
enable_language(C)
project(elibgit2)

set(BUILD_SHARED_LIBS OFF)
set(POSITION_INDEPENDENT_CODE ON)

add_subdirectory(libgit2)

find_library(git2 libgit2.a)
add_subdirectory(src)
