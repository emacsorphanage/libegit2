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
