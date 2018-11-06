#include "egit.h"

#ifndef EGIT_CONFIG_H
#define EGIT_CONFIG_H

EGIT_DEFUN(config_snapshot, emacs_value _config);

EGIT_DEFUN(config_get_string, emacs_value _config, emacs_value _name);

#endif /* EGIT_CONFIG_H */
