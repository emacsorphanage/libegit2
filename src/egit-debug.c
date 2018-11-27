#include "egit.h"
#include "interface.h"
#include "egit-debug.h"


EGIT_DOC(_refcount, "OBJ", "Return the reference count of OBJ.");
emacs_value egit__refcount(emacs_env *env, emacs_value val)
{
    EM_ASSERT_USER_PTR(val);
    egit_object *wrapper = (egit_object*) EM_EXTRACT_USER_PTR(val);
    return EM_INTEGER(wrapper->refcount);
}

EGIT_DOC(_wrapper, "OBJ", "Return the address of the wrapper object.");
emacs_value egit__wrapper(emacs_env *env, emacs_value val)
{
    EM_ASSERT_USER_PTR(val);
    egit_object *wrapper = (egit_object*) EM_EXTRACT_USER_PTR(val);
    return EM_INTEGER((ptrdiff_t) wrapper);
}

EGIT_DOC(_wrapped, "OBJ", "Return the address of the wrapped object.");
emacs_value egit__wrapped(emacs_env *env, emacs_value val)
{
    EM_ASSERT_USER_PTR(val);
    egit_object *wrapper = (egit_object*) EM_EXTRACT_USER_PTR(val);
    return EM_INTEGER((ptrdiff_t) wrapper->ptr);
}

EGIT_DOC(_parent_wrapper, "OBJ", "Return the address of the parent wrapper object.");
emacs_value egit__parent_wrapper(emacs_env *env, emacs_value val)
{
    EM_ASSERT_USER_PTR(val);
    egit_object *wrapper = (egit_object*) EM_EXTRACT_USER_PTR(val);
    return EM_INTEGER((ptrdiff_t) wrapper->parent);
}
