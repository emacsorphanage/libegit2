#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-options.h"
#include "interface.h"
#include "egit-merge.h"


EGIT_DOC(merge, "REPO HEADS &optional MERGE-OPTIONS CHECKOUT-OPTIONS",
         "Merge HEADS (a list of annotated commits) into the HEAD of REPO.\n"
         "For CHECKOUT-OPTIONS, see `libgit-checkout-head'.\n"
         "MERGE-OPTIONS is an alist with the following keys:\n"
         "- `find-renames': if non-nil, detect renames, enabling the ability\n"
         "     to merge between modified and renamed files\n"
         "- `fail-on-conflict': if non-nil, exit immediately on conflict without\n"
         "     continuing with resolution\n"
         "- `skip-reuc': if non-nil, do not write the REUC extension on the\n"
         "     generated index\n"
         "- `no-recursive': if non-nil, do not build a recursive merge base\n"
         "     in case of multiple merge bases, and instead simply use the first\n"
         "     base"
         "- `rename-threshold': similarity above which to consider a file to be\n"
         "     renamed (default 50)\n"
         "- `target-limit': maximum similarity sources to examine for renames\n"
         "     (default 200); this setting overrides the merge.renameLimit setting\n"
         "- `recursion-limit': maximum number of times to merge common ancestors\n"
         "     to build a virtual merge base when faced with criss-cross merges.\n"
         "     When the limit is reached, the next ancestor will be used instead of\n"
         "     attempting to merge it. Default unlimited.\n"
         "- `default-driver': string denoting the merge driver (default \"text\")\n"
         "- `file-favor': can be any of the symbols `normal' (default): produce a\n"
         "     conflict in case a region of a file is changed in both branches,\n"
         "     `ours': choose our side, don't produce a conflict, `theirs': choose\n"
         "     their side, don't produce a conflict, `union': pick each unique line\n"
         "     from each side, don't produce a conflict\n"
         "- `file-flags': an alist of flags (boolean options) with the following\n"
         "     keys:\n"
         "     - `style-merge': create standard conflicted merge files\n"
         "     - `style-diff3': create diff3-style files\n"
         "     - `simplify-alnum': condense non-alphanumeric regions\n"
         "     - `ignore-whitespace': ignore all whitespace\n"
         "     - `ignore-whitespace-change': ignore changes in whitespace\n"
         "     - `ignore-whitespace-eol': ignore whitespace at end of line\n"
         "     - `patience': use the patience diff algorithm\n"
         "     - `minimal': take extra time to find a minimal diff");
emacs_value egit_merge(
    emacs_env *env, emacs_value _repo, emacs_value _heads,
    emacs_value _merge_opts, emacs_value _checkout_opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    ptrdiff_t i = 0;
    ptrdiff_t nheads = egit_assert_list(env, EGIT_ANNOTATED_COMMIT, esym_libgit_annotated_commit_p, _heads);
    if (nheads < 0)
        return esym_nil;
    const git_annotated_commit *heads[nheads];
    {
        EM_DOLIST(h, _heads, get_heads);
        heads[i++] = EGIT_EXTRACT(h);
        EM_DOLIST_END(get_heads);
    }

    git_merge_options merge_opts;
    egit_merge_options_parse(env, _merge_opts, &merge_opts);
    EM_RETURN_NIL_IF_NLE();

    git_checkout_options checkout_opts;
    egit_checkout_options_parse(env, _checkout_opts, &checkout_opts);
    EM_RETURN_NIL_IF_NLE();

    int retval = git_merge(repo, heads, nheads, &merge_opts, &checkout_opts);
    egit_checkout_options_release(&checkout_opts);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(merge_analysis, "REPO HEADS",
         "Analyze the effects of merging HEADS into the current HEAD of repo.\n"
         "HEADS should be a list of annotated commits.\n"
         "The return value is a cons (ANALYSIS . PREFERENCE) where ANALYSIS is\n"
         "a list with the following symbols:\n"
         "- `normal': a normal merge is possible\n"
         "- `up-to-date': inputs are reachable from HEAD, no merge necessary\n"
         "- `fastforward': a fast-forward merge is possible\n"
         "- `unborn': HEAD is unborn, no merge possible\n\n"
         "PREFERENCE indicates the merge.ff setting, and can be either `nil',\n"
         "`no-fastforward' or `fastforward-only'.");
emacs_value egit_merge_analysis(emacs_env *env, emacs_value _repo, emacs_value _heads)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    ptrdiff_t i = 0;
    ptrdiff_t nheads = egit_assert_list(env, EGIT_ANNOTATED_COMMIT, esym_libgit_annotated_commit_p, _heads);
    if (nheads < 0)
        return esym_nil;
    const git_annotated_commit *heads[nheads];
    {
        EM_DOLIST(h, _heads, get_heads);
        heads[i++] = EGIT_EXTRACT(h);
        EM_DOLIST_END(get_heads);
    }

    git_merge_analysis_t analysis;
    git_merge_preference_t preference;
    int retval = git_merge_analysis(&analysis, &preference, repo, heads, nheads);
    EGIT_CHECK_ERROR(retval);

    emacs_value _analysis = em_getlist_merge_analysis(env, analysis);

    // These are bit flags but only one can be set
    emacs_value _preference = em_findenum_merge_preference(preference);

    return em_cons(env, _analysis, _preference);
}

EGIT_DOC(merge_base, "REPO IDS",
         "Find the best merge base between commits given by the list IDS.\n"
         "Returns a commit ID.");
emacs_value egit_merge_base(emacs_env *env, emacs_value _repo, emacs_value _ids)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    ptrdiff_t i = 0, nids = em_assert_list(env, esym_stringp, _ids);
    git_oid ids[nids];
    {
        EM_DOLIST(id, _ids, get_ids);
        EGIT_EXTRACT_OID(id, ids[i]);
        i++;
        EM_DOLIST_END(get_ids);
    }

    git_oid out;
    int retval;
    if (nids == 2)
        retval = git_merge_base(&out, repo, &ids[0], &ids[1]);
    else
        retval = git_merge_base_many(&out, repo, nids, ids);
    EGIT_CHECK_ERROR(retval);

    const char *oid_s = git_oid_tostr_s(&out);
    return EM_STRING(oid_s);
}

EGIT_DOC(merge_base_octopus, "REPO IDS",
         "Find a merge base in preparation for an octopus merge.\n"
         "Returns a commit ID.");
emacs_value egit_merge_base_octopus(emacs_env *env, emacs_value _repo, emacs_value _ids)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    ptrdiff_t i = 0, nids = em_assert_list(env, esym_stringp, _ids);
    git_oid ids[nids];
    {
        EM_DOLIST(id, _ids, get_ids);
        EGIT_EXTRACT_OID(id, ids[i]);
        i++;
        EM_DOLIST_END(get_ids);
    }

    git_oid out;
    int retval = git_merge_base_octopus(&out, repo, nids, ids);
    EGIT_CHECK_ERROR(retval);

    const char *oid_s = git_oid_tostr_s(&out);
    return EM_STRING(oid_s);
}

EGIT_DOC(merge_bases, "REPO IDS",
         "Find all merge bases between commits given by the list IDS.\n"
         "Returns a list of commit IDs.");
emacs_value egit_merge_bases(emacs_env *env, emacs_value _repo, emacs_value _ids)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    ptrdiff_t i = 0, nids = em_assert_list(env, esym_stringp, _ids);
    git_oid ids[nids];
    {
        EM_DOLIST(id, _ids, get_ids);
        EGIT_EXTRACT_OID(id, ids[i]);
        i++;
        EM_DOLIST_END(get_ids);
    }

    git_oidarray out;
    int retval;
    if (nids == 2)
        retval = git_merge_bases(&out, repo, &ids[0], &ids[1]);
    else
        retval = git_merge_bases_many(&out, repo, nids, ids);
    EGIT_CHECK_ERROR(retval);

    emacs_value ret = esym_nil;
    for (size_t i = out.count; i > 0; i--) {
        const char *oid_s = git_oid_tostr_s(&out.ids[i-1]);
        ret = em_cons(env, EM_STRING(oid_s), ret);
    }
    git_oidarray_free(&out);

    return ret;
}
