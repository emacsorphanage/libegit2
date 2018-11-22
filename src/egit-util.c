#include "git2.h"
#include "interface.h"

bool egit_strarray_from_list(git_strarray *array, emacs_env *env, emacs_value list)
{
    array->count = 0;
    array->strings = NULL;

    ptrdiff_t nelems = em_assert_list(env, em_stringp, list);
    if (nelems < 0)
        return false;
    if (nelems == 0)
        return true;

    array->count = nelems;
    array->strings = (char**) malloc(nelems * sizeof(char*));
    for (ptrdiff_t i = 0; i < nelems; i++) {
        emacs_value car = em_car(env, list);
        array->strings[i] = EM_EXTRACT_STRING(car);
        list = em_cdr(env, list);
    }

    return true;
}

void egit_strarray_dispose(git_strarray *array)
{
    if (array->strings) {
        for (size_t i = 0; i < array->count; i++)
            free(array->strings[i]);
        free(array->strings);
    }
}
