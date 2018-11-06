#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-config.h"


// =============================================================================
// Constructors

EGIT_DOC(config_snapshot, "CONFIG", "Create a consistent snapshot of CONFIG.");
emacs_value egit_config_snapshot(emacs_env *env, emacs_value _config)
{
    EGIT_ASSERT_CONFIG(_config);
    git_config *config = EGIT_EXTRACT(_config);
    git_config *snapshot;
    int retval = git_config_snapshot(&snapshot, config);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CONFIG, snapshot);
}


// =============================================================================
// Getters

EGIT_DOC(config_get_string, "CONFIG NAME",
         "Get the value of NAME in CONFIG as a string.\n"
         "CONFIG must be a snapshot, see `libgit-config-snapshot'.");
emacs_value egit_config_get_string(emacs_env *env, emacs_value _config, emacs_value _name)
{
    EGIT_ASSERT_CONFIG(_config);
    EGIT_ASSERT_STRING(_name);
    git_config *config = EGIT_EXTRACT(_config);
    char *name = EGIT_EXTRACT_STRING(_name);
    const char *value;
    int retval = git_config_get_string(&value, config, name);
    free(name);
    EGIT_CHECK_ERROR(retval);
    return env->make_string(env, value, strlen(value));
}

EGIT_DOC(config_lock, "CONFIG", "Lock the highest priority backend in CONFIG.");
emacs_value egit_config_lock(emacs_env *env, emacs_value _config)
{
    EGIT_ASSERT_CONFIG(_config);
    git_config *config = EGIT_EXTRACT(_config);
    git_transaction *trans;
    int retval = git_config_lock(&trans, config);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_TRANSACTION, trans);
}


// =============================================================================
// Setters

EGIT_DOC(config_set_string, "CONFIG NAME VALUE", "Set the value of NAME in CONFIG to VALUE.");
emacs_value egit_config_set_string(emacs_env *env, emacs_value _config, emacs_value _name, emacs_value _value)
{
    EGIT_ASSERT_CONFIG(_config);
    EGIT_ASSERT_STRING(_name);
    EGIT_ASSERT_STRING(_value);
    git_config *config = EGIT_EXTRACT(_config);
    const char *name = EGIT_EXTRACT_STRING(_name);
    const char *value = EGIT_EXTRACT_STRING(_value);
    int retval = git_config_set_string(config, name, value);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}
