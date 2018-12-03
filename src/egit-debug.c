#include "egit.h"
#include "interface.h"
#include "egit-debug.h"


typedef struct ptr_cell_s ptr_cell;

struct ptr_cell_s {
    void *ptr;
    ptr_cell *next;
};

static ptr_cell *alloc_stack = NULL;
static ptr_cell *finalize_stack = NULL;
static ptr_cell *free_stack = NULL;

static void push_stack(ptr_cell **stack, void *ptr)
{
    ptr_cell *cell = (ptr_cell*) malloc(sizeof(ptr_cell));
    cell->ptr = ptr;
    cell->next = *stack;
    *stack = cell;
}

static void *pop_stack(ptr_cell **stack)
{
    ptr_cell *cell = *stack;
    if (!cell)
        return NULL;

    *stack = cell->next;
    void *retval = cell->ptr;
    free(cell);
    return retval;
}

static emacs_value convert_stack(emacs_env *env, ptr_cell **stack)
{
    emacs_value list = esym_nil;
    void *ptr;
    while ((ptr = pop_stack(stack)))
        list = em_cons(env, EM_INTEGER((ptrdiff_t) ptr), list);
    return list;
}

void egit_signal_alloc(void *ptr) { push_stack(&alloc_stack, ptr); }
void egit_signal_finalize(void *ptr) { push_stack(&finalize_stack, ptr); }
void egit_signal_free(void *ptr) { push_stack(&free_stack, ptr); }

EGIT_DOC(_allocs, "", "Return a list of wrapper pointers that have been allocated.");
emacs_value egit__allocs(emacs_env *env) { return convert_stack(env, &alloc_stack); }

EGIT_DOC(_finalizes, "", "Return a list of wrapper pointers that have been finalized.");
emacs_value egit__finalizes(emacs_env *env) { return convert_stack(env, &finalize_stack); }

EGIT_DOC(_frees, "", "Return a list of wrapper pointers that have been freed.");
emacs_value egit__frees(emacs_env *env) { return convert_stack(env, &free_stack); }

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
