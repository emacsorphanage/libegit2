file(GLOB ELIBGIT2_SRCS *.c)

add_library(egit2 SHARED ${ELIBGIT2_SRCS})
set_target_properties(egit2 PROPERTIES C_STANDARD 99)

# Emacs looks for .so on linux and OSX.
# By default CMake builds .dylib on OSX, so override that.
# Windows is TODO
set_target_properties(egit2 PROPERTIES SUFFIX .so)

target_link_libraries(egit2 git2)
target_include_directories(egit2 SYSTEM PRIVATE "${libgit2_SOURCE_DIR}/include")
target_include_directories(egit2 SYSTEM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../uthash")
target_compile_options(egit2 PRIVATE -Wall -Wextra)

add_custom_command(TARGET egit2 POST_BUILD COMMAND
  ${CMAKE_COMMAND} -E copy $<TARGET_FILE:egit2> "${CMAKE_CURRENT_BINARY_DIR}/..")
