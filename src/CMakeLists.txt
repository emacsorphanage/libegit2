file(GLOB ELIBGIT2_SRCS *.c)

add_library(elibgit2 SHARED ${ELIBGIT2_SRCS})

target_link_libraries(elibgit2 git2)
