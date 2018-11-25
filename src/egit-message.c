#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-message.h"


EGIT_DOC(message_prettify, "MESSAGE &optional COMMENT-CHAR",
         "Prettify MESSAGE: clean up whitespace and ensure trailing newline.\n"
         "If COMMENT-CHAR is non-nil, remove lines starting with it.");
emacs_value egit_message_prettify(emacs_env *env, emacs_value _msg, emacs_value _comment)
{
    EM_ASSERT_STRING(_msg);

    char comment_char = 0;
    if (EM_EXTRACT_BOOLEAN(_comment)) {
        EM_ASSERT_INTEGER(_comment);
        comment_char = EM_EXTRACT_INTEGER(_comment);
    }

    char *msg = EM_EXTRACT_STRING(_msg);
    git_buf buf = {NULL, 0, 0};
    int retval = git_message_prettify(&buf, msg, EM_EXTRACT_BOOLEAN(_comment), comment_char);
    free(msg);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_BUF_AS_STRING(buf);
}

EGIT_DOC(message_trailers, "MESSAGE", "Return trailers in MESSAGE as a list of cons cells.");
emacs_value egit_message_trailers(emacs_env *env, emacs_value _msg)
{
    EM_ASSERT_STRING(_msg);

    git_message_trailer_array trailers;
    char *msg = EM_EXTRACT_STRING(_msg);
    int retval = git_message_trailers(&trailers, msg);
    free(msg);
    EGIT_CHECK_ERROR(retval);

    emacs_value cells[trailers.count];
    for (size_t i = 0; i < trailers.count; i++)
        cells[i] = em_cons(env, EM_STRING(trailers.trailers[i].key), EM_STRING(trailers.trailers[i].value));
    git_message_trailer_array_free(&trailers);
    return em_list(env, cells, trailers.count);
}
