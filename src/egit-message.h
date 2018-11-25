#include "egit.h"

#ifndef EGIT_MESSAGE_H
#define EGIT_MESSAGE_H

EGIT_DEFUN(message_prettify, emacs_value _msg, emacs_value _comment_char);
EGIT_DEFUN(message_trailers, emacs_value _msg);

#endif /* EGIT_MESSAGE_H */
