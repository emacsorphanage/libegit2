file(GLOB ELIBGIT2_SRCS *.c)

add_library(egit2 SHARED ${ELIBGIT2_SRCS})

target_link_libraries(egit2 git2)
target_include_directories(egit2 SYSTEM PRIVATE "${libgit2_SOURCE_DIR}/include")
target_include_directories(egit2 SYSTEM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../uthash")
target_compile_options(egit2 PRIVATE -Wall -Wextra)

add_custom_command(TARGET egit2 POST_BUILD COMMAND
  ${CMAKE_COMMAND} -E copy $<TARGET_FILE:egit2> "${CMAKE_CURRENT_BINARY_DIR}/..")
