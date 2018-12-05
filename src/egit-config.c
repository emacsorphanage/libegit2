#include <string.h>
#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-config.h"


// =============================================================================
// Constructors

EGIT_DOC(config_new, "", "Open a new empty config object.");
emacs_value egit_config_new(emacs_env *env)
{
    git_config *config;
    int retval = git_config_new(&config);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CONFIG, config, NULL);
}

EGIT_DOC(config_open_default, "",
         "Finds the global, XDG and system configuration files and opens them\n"
         "in a single prioritized config object that can be used for accessing\n"
         "out-of-repo config.");
emacs_value egit_config_open_default(emacs_env *env)
{
    git_config *config;
    int retval = git_config_open_default(&config);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CONFIG, config, NULL);
}

EGIT_DOC(config_open_global, "CONFIG",
         "Open the global/XDG configuration file according to git's rules.");
emacs_value egit_config_open_global(emacs_env *env, emacs_value _config)
{
    EGIT_ASSERT_CONFIG(_config);
    git_config *config = EGIT_EXTRACT(_config);
    git_config *new;
    int retval = git_config_open_global(&new, config);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CONFIG, new, NULL);
}

EGIT_DOC(config_open_level, "CONFIG &optional LEVEL",
         "Open a single config file in CONFIG at the given LEVEL.\n"
         "LEVEL, if given, is the priority level: one of `programdata'\n"
         "`system', `xdg', `global', `local', `app' or nil.\n"
         "If LEVEL is nil, the highest priority file will be opened.");
emacs_value egit_config_open_level(emacs_env *env, emacs_value _config, emacs_value _level)
{
    EGIT_ASSERT_CONFIG(_config);

    git_config_level_t level;
    if (!em_findsym_config_level(&level, env, _level, true))
        return esym_nil;

    git_config *config = EGIT_EXTRACT(_config);
    git_config *new;
    int retval = git_config_open_level(&new, config, level);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CONFIG, new, NULL);
}

EGIT_DOC(config_open_ondisk, "PATH", "Open a config file at PATH.");
emacs_value egit_config_open_ondisk(emacs_env *env, emacs_value _path)
{
    EM_ASSERT_STRING(_path);
    EM_NORMALIZE_PATH(_path);
    char *path = EM_EXTRACT_STRING(_path);
    git_config *config;
    int retval = git_config_open_ondisk(&config, path);
    free(path);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CONFIG, config, NULL);
}

EGIT_DOC(config_snapshot, "CONFIG", "Create a consistent snapshot of CONFIG.");
emacs_value egit_config_snapshot(emacs_env *env, emacs_value _config)
{
    EGIT_ASSERT_CONFIG(_config);
    git_config *config = EGIT_EXTRACT(_config);
    git_config *snapshot;
    int retval = git_config_snapshot(&snapshot, config);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CONFIG, snapshot, NULL);
}


// =============================================================================
// Getters

EGIT_DOC(config_get_bool, "CONFIG NAME",
         "Get the value of NAME in CONFIG as a boolean (`t' or `nil').\n"
         "CONFIG must be a snapshot, see `libgit-config-snapshot'.");
emacs_value egit_config_get_bool(emacs_env *env, emacs_value _config, emacs_value _name)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_name);
    git_config *config = EGIT_EXTRACT(_config);
    char *name = EM_EXTRACT_STRING(_name);
    int value;
    int retval = git_config_get_bool(&value, config, name);
    free(name);
    EGIT_CHECK_ERROR(retval);
    return value ? esym_t : esym_nil;
}

EGIT_DOC(config_get_int, "CONFIG NAME",
         "Get the value of NAME in CONFIG as an integer.\n"
         "CONFIG must be a snapshot, see `libgit-config-snapshot'.");
emacs_value egit_config_get_int(emacs_env *env, emacs_value _config, emacs_value _name)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_name);
    git_config *config = EGIT_EXTRACT(_config);
    char *name = EM_EXTRACT_STRING(_name);
    int64_t value;
    int retval = git_config_get_int64(&value, config, name);
    free(name);
    EGIT_CHECK_ERROR(retval);
    return EM_INTEGER(value);
}

EGIT_DOC(config_get_path, "CONFIG NAME",
         "Get the value of NAME in CONFIG as a string path.\n"
         "This is similar to `libgit-config-get-string' except the path\n"
         "is automatically normalized.\n"
         "CONFIG must be a snapshot, see `libgit-config-snapshot'.");
emacs_value egit_config_get_path(emacs_env *env, emacs_value _config, emacs_value _name)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_name);
    git_config *config = EGIT_EXTRACT(_config);
    char *name = EM_EXTRACT_STRING(_name);

    git_buf buf = {0};
    int retval = git_config_get_path(&buf, config, name);
    free(name);
    EGIT_CHECK_ERROR(retval);

    // git_config_get_path does some normalization,
    // but I trust Emacs' path normalization to be more thorough.
    emacs_value ret = env->make_string(env, buf.ptr, buf.size);
    git_buf_dispose(&buf);
    EM_NORMALIZE_PATH(ret);
    return ret;
}

EGIT_DOC(config_get_string, "CONFIG NAME",
         "Get the value of NAME in CONFIG as a string.\n"
         "CONFIG must be a snapshot, see `libgit-config-snapshot'.");
emacs_value egit_config_get_string(emacs_env *env, emacs_value _config, emacs_value _name)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_name);
    git_config *config = EGIT_EXTRACT(_config);
    char *name = EM_EXTRACT_STRING(_name);
    const char *value;
    int retval = git_config_get_string(&value, config, name);
    free(name);
    EGIT_CHECK_ERROR(retval);
    return EM_STRING(value);
}

EGIT_DOC(config_lock, "CONFIG", "Lock the highest priority backend in CONFIG.");
emacs_value egit_config_lock(emacs_env *env, emacs_value _config)
{
    EGIT_ASSERT_CONFIG(_config);
    git_config *config = EGIT_EXTRACT(_config);
    git_transaction *trans;
    int retval = git_config_lock(&trans, config);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_TRANSACTION, trans, NULL);
}


// =============================================================================
// Setters

EGIT_DOC(config_set_bool, "CONFIG NAME VALUE", "Set the value of NAME in CONFIG to VALUE.");
emacs_value egit_config_set_bool(emacs_env *env, emacs_value _config, emacs_value _name, emacs_value _value)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_name);
    git_config *config = EGIT_EXTRACT(_config);
    const char *name = EM_EXTRACT_STRING(_name);
    int value = EM_EXTRACT_BOOLEAN(_value);
    int retval = git_config_set_bool(config, name, value);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(config_set_int, "CONFIG NAME VALUE", "Set the value of NAME in CONFIG to VALUE.");
emacs_value egit_config_set_int(emacs_env *env, emacs_value _config, emacs_value _name, emacs_value _value)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_name);
    EM_ASSERT_INTEGER(_value);
    git_config *config = EGIT_EXTRACT(_config);
    const char *name = EM_EXTRACT_STRING(_name);
    int64_t value = EM_EXTRACT_INTEGER(_value);
    int retval = git_config_set_int64(config, name, value);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(config_set_string, "CONFIG NAME VALUE", "Set the value of NAME in CONFIG to VALUE.");
emacs_value egit_config_set_string(emacs_env *env, emacs_value _config, emacs_value _name, emacs_value _value)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_value);
    git_config *config = EGIT_EXTRACT(_config);
    const char *name = EM_EXTRACT_STRING(_name);
    const char *value = EM_EXTRACT_STRING(_value);
    int retval = git_config_set_string(config, name, value);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}


// =============================================================================
// Operations

EGIT_DOC(config_add_file_ondisk, "CONFIG PATH &optional LEVEL REPO FORCE",
         "Add a new config file at PATH to the given CONFIG.\n"
         "LEVEL, if given, is the priority level: one of `programdata'\n"
         "`system', `xdg', `global', `local' or `app' (default).\n\n"
         "If REPO is given, parse optional includes.\n"
         "If FORCE is non-nil, replace the config at the given level.");
emacs_value egit_config_add_file_ondisk(
    emacs_env *env, emacs_value _config, emacs_value _path,
    emacs_value _level, emacs_value _repo, emacs_value _force)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_path);
    EM_NORMALIZE_PATH(_path);
    if (EM_EXTRACT_BOOLEAN(_repo))
        EGIT_ASSERT_REPOSITORY(_repo);

    git_config_level_t level;
    if (!em_findsym_config_level(&level, env, _level, true))
        return esym_nil;

    git_config *config = EGIT_EXTRACT(_config);
    char *path = EM_EXTRACT_STRING(_path);
    git_repository *repo = EGIT_EXTRACT_OR_NULL(_repo);
    int force = EM_EXTRACT_BOOLEAN(_force);

    int retval = git_config_add_file_ondisk(config, path, level, repo, force);
    free(path);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(config_delete_entry, "CONFIG NAME",
         "Delete the entry named NAME from the file in CONFIG with the highest level.");
emacs_value egit_config_delete_entry(emacs_env *env, emacs_value _config, emacs_value _name)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_name);

    git_config *config = EGIT_EXTRACT(_config);
    char *name = EM_EXTRACT_STRING(_name);
    int retval = git_config_delete_entry(config, name);
    free(name);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(config_delete_multivar, "CONFIG NAME REGEXP",
         "Delete all values of NAME in CONFIG matching REGEXP.");
emacs_value egit_config_delete_multivar(
    emacs_env *env, emacs_value _config, emacs_value _name, emacs_value _regexp)
{
    EGIT_ASSERT_CONFIG(_config);
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_regexp);

    git_config *config = EGIT_EXTRACT(_config);
    char *name = EM_EXTRACT_STRING(_name);
    char *regexp = EM_EXTRACT_STRING(_regexp);
    int retval = git_config_delete_multivar(config, name, regexp);
    free(name);
    free(regexp);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}


// =============================================================================
// Miscellaneous

EGIT_DOC(config_find_global, "", "Get the path to the global config file.");
emacs_value egit_config_find_global(emacs_env *env)
{
    git_buf out = {0};
    int retval = git_config_find_global(&out);
    EGIT_CHECK_ERROR(retval);
    emacs_value ret = env->make_string(env, out.ptr, out.size);
    EM_NORMALIZE_PATH(ret);
    git_buf_dispose(&out);
    return ret;
}

EGIT_DOC(config_find_programdata, "", "Get the path to the config file in ProgramData.");
emacs_value egit_config_find_programdata(emacs_env *env)
{
    git_buf out = {0};
    int retval = git_config_find_programdata(&out);
    EGIT_CHECK_ERROR(retval);
    emacs_value ret = env->make_string(env, out.ptr, out.size);
    EM_NORMALIZE_PATH(ret);
    git_buf_dispose(&out);
    return ret;
}

EGIT_DOC(config_find_system, "", "Get the path to the system config file.");
emacs_value egit_config_find_system(emacs_env *env)
{
    git_buf out = {0};
    int retval = git_config_find_system(&out);
    EGIT_CHECK_ERROR(retval);
    emacs_value ret = env->make_string(env, out.ptr, out.size);
    EM_NORMALIZE_PATH(ret);
    git_buf_dispose(&out);
    return ret;
}

EGIT_DOC(config_find_xdg, "", "Get the path to the XDG config file.");
emacs_value egit_config_find_xdg(emacs_env *env)
{
    git_buf out = {0};
    int retval = git_config_find_xdg(&out);
    EGIT_CHECK_ERROR(retval);
    emacs_value ret = env->make_string(env, out.ptr, out.size);
    EM_NORMALIZE_PATH(ret);
    git_buf_dispose(&out);
    return ret;
}
