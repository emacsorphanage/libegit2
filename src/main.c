#include "emacs-module.h"
#include "git2.h"

#include "egit.h"
#include "interface.h"

int plugin_is_GPL_compatible;


static bool initialized = false;

/**
 * Initialize the libegit2 module.
 * This is only done once, no matter how many times it's called.
 */
int emacs_module_init(struct emacs_runtime *ert)
{
    if (initialized)
        return 0;

    emacs_env *env = ert->get_environment(ert);

    // Initialize libgit2
    git_libgit2_init();

    // Initialize our own interface to Emacs
    em_init(env);

    // Define all lisp-callable functions
    egit_init(env);

    em_provide(env, "libegit2");
    initialized = true;
    return 0;
}
