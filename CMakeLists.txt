cmake_minimum_required(VERSION 3.0)
enable_language(C)
project(elibgit2)

set(CMAKE_POSITION_INDEPENDENT_CODE TRUE CACHE BOOL "pic" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "shared" FORCE)
set(BUILD_CLAR OFF CACHE BOOL "clar" FORCE)

add_subdirectory(libgit2)

find_library(git2 libgit2.a)
add_subdirectory(src)

enable_testing()
add_test(NAME libegit2_repository
  COMMAND cask exec ert-runner test/repo-test.el
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
