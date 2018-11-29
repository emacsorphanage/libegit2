#include "egit.h"

#ifndef EGIT_CONFIG_H
#define EGIT_CONFIG_H

EGIT_DEFUN_0(config_new);
EGIT_DEFUN_0(config_open_default);
EGIT_DEFUN(config_open_global, emacs_value _config);
EGIT_DEFUN(config_open_level, emacs_value _config, emacs_value _level);
EGIT_DEFUN(config_open_ondisk, emacs_value _path);
EGIT_DEFUN(config_snapshot, emacs_value _config);

EGIT_DEFUN(config_get_bool, emacs_value _config, emacs_value _name);
EGIT_DEFUN(config_get_int, emacs_value _config, emacs_value _name);
EGIT_DEFUN(config_get_path, emacs_value _config, emacs_value _name);
EGIT_DEFUN(config_get_string, emacs_value _config, emacs_value _name);
EGIT_DEFUN(config_lock, emacs_value _config);

EGIT_DEFUN(config_set_bool, emacs_value _config, emacs_value _name, emacs_value _value);
EGIT_DEFUN(config_set_int, emacs_value _config, emacs_value _name, emacs_value _value);
EGIT_DEFUN(config_set_string, emacs_value _config, emacs_value _name, emacs_value _value);

EGIT_DEFUN(config_add_file_ondisk, emacs_value _config, emacs_value _path,
           emacs_value _level, emacs_value _repo, emacs_value _force);
EGIT_DEFUN(config_delete_entry, emacs_value _config, emacs_value _name);
EGIT_DEFUN(config_delete_multivar, emacs_value _config, emacs_value _name, emacs_value _regexp);

EGIT_DEFUN_0(config_find_global);
EGIT_DEFUN_0(config_find_programdata);
EGIT_DEFUN_0(config_find_system);
EGIT_DEFUN_0(config_find_xdg);

#endif /* EGIT_CONFIG_H */
