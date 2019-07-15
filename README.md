# libgit2 bindings for Emacs

[![Travis build status](https://travis-ci.org/magit/libegit2.svg?branch=master "Build Status")](https://travis-ci.org/magit/libegit2)
[![Appveyor build status](https://ci.appveyor.com/api/projects/status/jq45m47uubafv9kq/branch/master?svg=true)](https://ci.appveyor.com/project/tarsius/libegit2/branch/master)

This is an *experimental* module for libgit2 bindings to Emacs, intended to boost the performance of
[magit](https://github.com/magit/magit).

Other work in this direction:
- [ksjogo/emacs-libgit2](https://github.com/ksjogo/emacs-libgit2) in C, has been dormant for more
  than a year.
- [ubolonton/magit-libgit2](https://github.com/ubolonton/magit-libgit2) in Rust.

This module is written in C, and aims to be a thin wrapper around libgit2. That means that all
functions in the [libgit2 reference](https://libgit2.github.com/libgit2/#HEAD) should translate
more-or-less directly to Emacs, in the following sense:

- Function names are the same, except with underscores replaced by hyphens. The prefix is changed
  from `git-` to `libgit-`.
- Predicate functions are given a `-p` suffix, and words like "is" are removed,
  e.g. `git_repository_is_bare` becomes `libgit-repository-bare-p`.
- Output parameters become return values.
- Error codes become error signals (type `giterr`).
- Return types map to their natural Emacs counterparts, or opaque user pointers when not applicable
  (e.g. for `git-???` structures). Exceptions: `git-oid` and `git-buf` types are converted to Emacs
  strings.
- Boolean parameters or pointers towards the end of argument lists whose natural default value is
  false or NULL will be made optional.

Quality-of-life convenience functionality is better implemented in Emacs Lisp than in C.

## Building

There is a loader file written in Emacs Lisp that will build the module for you, but the
`git submodule` steps need to be run manually.

```
git submodule init
git submodule update
mkdir build
cd build
cmake ..
make
```

If you're on OSX and using Macports, you may need to set `CMAKE_PREFIX_PATH` to avoid linking
against the wrong libiconv. For example,

```
cmake -DCMAKE_PREFIX_PATH=/opt/local ..
```

## Testing

Ensure that you have [Cask](https://github.com/cask/cask) installed.

```
cask install
cd build
make test
```

To see more output for debugging new tests you can specify more verbose output.

```
make test ARGS=-V
```

## Using

Ensure that `libgit.el` is somewhere in your load path. Then

```elisp
(require 'libgit)
```

If the dynamic module was not already built, you should be asked to do it manually.

If you use [Borg](https://github.com/emacscollective/borg), then use the following `.gitmodules` entry.

```
[submodule "libgit"]
    path = lib/libgit
    url = git@github.com:magit/libegit2.git
    build-step = make
```

## Contributing

### Adding a function

1. Find the section that the function belongs to (i.e. `git_SECTION_xyz`).
2. Create, if necessary, `src/egit-SECTION.h` and `src/egit-SECTION.c`.
3. In `src/egit-SECTION.h`, declare the function with `EGIT_DEFUN`. See existing headers for
   examples.
4. In `src/egit-SECTION.c`, document the function with `EGIT_DOC`. See existing files for examples.
5. In `src/egit-SECTION.c`, implement the function. See existing files for examples.
   1. Always check argument types in the beginning. Use `EGIT_ASSERT` for this. These macros may return.
   2. Then, extract the data needed from `emacs_value`. This may involve allocating buffers for strings.
   3. Call the `libgit2` backend function.
   4. Free any memory you might need to free that was allocated in step 2.
   5. Check the error code if applicable with `EGIT_CHECK_ERROR`. This macro may return.
   6. Create return value and return.
6. In `src/egit.c`, create a `DEFUN` call in `egit_init`. You may need to include a new header.

### Adding a type

Sometimes a struct of type `git_???` may need to be returned to Emacs as an opaque user pointer.
To do this, we use a wrapper structure with a type information tag.

Some objects expose data that belong to other objects. In many cases, libgit2 keeps reference-counts
on these internally, but that's not always true. In particular, `git_repository` structs are not
reference-counted (altough the data-owning sub-objects like `git_odb` are). Neither are lightweight
public structs like `git_index_entry`, `git_diff_XYZ`, etc. In these cases, the *parent types* must
be reference-counted on our side, and the *child types* must keep a reference to the parent alive.

1. In `src/egit.h`, add an entry to the `egit_type` enum for the new type.
2. In `src/egit.h` add a new `EGIT_ASSERT` macro for the new type.
3. In `src/egit.c` add a new entry to the `egit_finalize` switch statement to free the structure. If
   the type is reference-counted, also add an entry to the decref switch statement.
4. In `src/egit.c` add a new entry to the `egit_typeof` switch statement.
5. In `src/egit.c` add a new type predicate by calling the `TYPECHECKER` macro.
6. In `src/egit.c` create a `DEFUN` call in `egit_init` for the type predicate.
7. In `src/interface.h` add two new symbols, `libgit-TYPE-p` and `TYPE`.
8. In `src/interface.c` initialize those symbols in the `em_init` function.

### Returning opaque pointers to Emacs

To create a new user pointer, call `egit_wrap` with arguments:

1. The `emacs_env*`
2. The type tag
3. The pointer to wrap
4. The parent wrapper, if applicable (note: this is an `egit_object*`, not a `git_XYZ*`)

To return an existing user pointer (usually by grabbing the parent field of an `egit_object*`), 
just increase the reference count and use the `EM_USER_PTR` macro. Do not do this for types that are
not reference-counted!

## Function list

This is a complete list of functions in libgit2. It therefore serves more or less as an upper bound
on the amount of work needed.

Legend:
- :heavy_check_mark: Function is implemented
- :x: Function should probably not be implemented (reason given)
- :grey_question: Undecided

Some functions are defined in libgit2 headers in the `sys` subdirectory, and are not reachable from
a standard include (i.e. `#include "git2.h"`). For now, we will skip those on the assumption that
they are more specialized.

Estimates (updated periodically):
- Implemented: 325 (41.8%)
- Should not implement: 169 (21.7%)
- To do: 284 (36.5%)
- Total: 778

### extra

These are functions that do not have a `libgit2` equivalent.

Type checkers and predicates:

- :heavy_check_mark: `git-typeof`
- :heavy_check_mark: `git-blame-p`
- :heavy_check_mark: `git-commit-p`
- :heavy_check_mark: `git-cred-p`
- :heavy_check_mark: `git-diff-p`
- :heavy_check_mark: `git-diff-delta-p`
- :heavy_check_mark: `git-diff-binary-p`
- :heavy_check_mark: `git-diff-hunk-p`
- :heavy_check_mark: `git-diff-line-p`
- :heavy_check_mark: `git-index-p`
- :heavy_check_mark: `git-index-entry-p`
- :heavy_check_mark: `git-object-p`
- :heavy_check_mark: `git-reference-p`
- :heavy_check_mark: `git-repository-p`
- :heavy_check_mark: `git-signature-p`
- :heavy_check_mark: `git-reference-direct-p`
- :heavy_check_mark: `git-reference-symbolic-p`
- :heavy_check_mark: `git-transaction-p`
- :heavy_check_mark: `git-tree-p`

Getters for public structs:

- :heavy_check_mark: `git-blame-hunk-commit-id`
- :heavy_check_mark: `git-blame-hunk-lines`
- :heavy_check_mark: `git-blame-hunk-orig-path`
- :heavy_check_mark: `git-blame-hunk-signature`
- :heavy_check_mark: `git-blame-hunk-start-line-number`
- :heavy_check_mark: `git-diff-delta-file-id`
- :heavy_check_mark: `git-diff-delta-file-path`
- :heavy_check_mark: `git-diff-delta-nfiles`
- :heavy_check_mark: `git-diff-delta-similarity`
- :heavy_check_mark: `git-diff-delta-status`
- :heavy_check_mark: `git-diff-delta-file-exists-p`
- :heavy_check_mark: `git-diff-hunk-header`
- :heavy_check_mark: `git-diff-hunk-lines`
- :heavy_check_mark: `git-diff-hunk-start`
- :heavy_check_mark: `git-diff-line-origin`
- :heavy_check_mark: `git-diff-line-lineno`
- :heavy_check_mark: `git-diff-line-content`
- :heavy_check_mark: `git-index-entry-path`
- :heavy_check_mark: `git-signature-name`
- :heavy_check_mark: `git-signature-email`
- :heavy_check_mark: `git-signature-time`

Iterators converted to map functions:

- :heavy_check_mark: `git-branch-foreach`
- :heavy_check_mark: `git-index-conflict-foreach`

### annotated

- :x: `git-annotated-commit-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-annotated-commit-from-fetchhead`
- :heavy_check_mark: `git-annotated-commit-from-ref`
- :heavy_check_mark: `git-annotated-commit-from-revspec`
- :heavy_check_mark: `git-annotated-commit-id`
- :heavy_check_mark: `git-annotated-commit-lookup`

### attr

- :grey_question: `git-attr-add-macro`
- :grey_question: `git-attr-cache-flush`
- :grey_question: `git-attr-foreach`
- :grey_question: `git-attr-get`
- :grey_question: `git-attr-get-many`
- :grey_question: `git-attr-value`

### blame

- :grey_question: `git-blame-buffer`
- :heavy_check_mark: `git-blame-file`
- :x: `git-blame-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-blame-get-hunk-byindex`
- :heavy_check_mark: `git-blame-get-hunk-byline`
- :heavy_check_mark: `git-blame-get-hunk-count`
- :x: `git-blame-init-options` (options are represented by an `alist`)

### blob

- :heavy_check_mark: `git-blob-create-frombuffer`
- :heavy_check_mark: `git-blob-create-fromdisk`
- :grey_question: `git-blob-create-fromstream`
- :grey_question: `git-blob-create-fromstream-commit`
- :heavy_check_mark: `git-blob-create-fromworkdir`
- :grey_question: `git-blob-dup`
- :heavy_check_mark: `git-blob-filtered-content`
- :x: `git-blob-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-blob-id`
- :heavy_check_mark: `git-blob-is-binary` (as `libgit-blob-binary-p`)
- :heavy_check_mark: `git-blob-lookup`
- :heavy_check_mark: `git-blob-lookup-prefix`
- :heavy_check_mark: `git-blob-owner`
- :heavy_check_mark: `git-blob-rawcontent`
- :heavy_check_mark: `git-blob-rawsize`

### branch

- :heavy_check_mark: `git-branch-create`
- :heavy_check_mark: `git-branch-create-from-annotated`
- :heavy_check_mark: `git-branch-delete`
- :heavy_check_mark: `git-branch-is-checked-out`
- :heavy_check_mark: `git-branch-is-head`
- :x: `git-branch-iterator-free`
- :x: `git-branch-iterator-new`
- :heavy_check_mark: `git-branch-lookup`
- :heavy_check_mark: `git-branch-move`
- :heavy_check_mark: `git-branch-name`
- :x: `git-branch-next`
- :heavy_check_mark: `git-branch-remote-name`
- :heavy_check_mark: `git-branch-set-upstream`
- :heavy_check_mark: `git-branch-upstream`
- :heavy_check_mark: `git-branch-upstream-name`
- :heavy_check_mark: `git-branch-upstream-remote`

### buf

Probably none of these functions are necessary, since we can expose buffers to Emacs as strings.

- :x: `git-buf-contains-nul`
- :x: `git-buf-free` (memory management shouldn't be exposed to Emacs)
- :x: `git-buf-grow`
- :x: `git-buf-is-binary`
- :x: `git-buf-set`

### checkout

- :heavy_check_mark: `git-checkout-head`
- :heavy_check_mark: `git-checkout-index`
- :x: `git-checkout-init-options`
- :heavy_check_mark: `git-checkout-tree`

### cherrypick

- :heavy_check_mark: `git-cherrypick`
- :heavy_check_mark: `git-cherrypick-commit`
- :x: `git-cherrypick-init-options`

### clone

- :heavy_check_mark: `git-clone`
- :grey_question: `git-clone-init-options`

### commit

- :grey_question: `git-commit-amend`
- :heavy_check_mark: `git-commit-author`
- :heavy_check_mark: `git-commit-body`
- :heavy_check_mark: `git-commit-committer`
- :heavy_check_mark: `git-commit-create`
- :grey_question: `git-commit-create-buffer`
- :x: `git-commit-create-from-callback` (in `sys`)
- :x: `git-commit-create-from-ids` (in `sys`)
- :x: `git-commit-create-v` (`git-commit-create` does the same)
- :grey_question: `git-commit-create-with-signature`
- :grey_question: `git-commit-dup`
- :grey_question: `git-commit-extract-signature`
- :x: `git-commit-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-commit-header-field`
- :heavy_check_mark: `git-commit-id`
- :heavy_check_mark: `git-commit-lookup`
- :heavy_check_mark: `git-commit-lookup-prefix`
- :heavy_check_mark: `git-commit-message`
- :grey_question: `git-commit-message-encoding`
- :grey_question: `git-commit-message-raw`
- :heavy_check_mark: `git-commit-nth-gen-ancestor`
- :heavy_check_mark: `git-commit-owner`
- :heavy_check_mark: `git-commit-parent`
- :heavy_check_mark: `git-commit-parent-id`
- :heavy_check_mark: `git-commit-parentcount`
- :grey_question: `git-commit-raw-header`
- :heavy_check_mark: `git-commit-summary`
- :heavy_check_mark: `git-commit-time`
- :x: `git-commit-time-offset` (included in `git-commit-time`)
- :heavy_check_mark: `git-commit-tree`
- :heavy_check_mark: `git-commit-tree-id`

### config

- :x: `git-config-add-backend` (in `sys`)
- :heavy_check_mark: `git-config-add-file-ondisk`
- :grey_question: `git-config-backend-foreach-match`
- :heavy_check_mark: `git-config-delete-entry`
- :heavy_check_mark: `git-config-delete-multivar`
- :x: `git-config-entry-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-config-find-global`
- :heavy_check_mark: `git-config-find-programdata`
- :heavy_check_mark: `git-config-find-system`
- :heavy_check_mark: `git-config-find-xdg`
- :grey_question: `git-config-foreach`
- :grey_question: `git-config-foreach-match`
- :x: `git-config-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-config-get-bool`
- :grey_question: `git-config-get-entry`
- :x: `git-config-get-int32` (don't need different integer types)
- :heavy_check_mark: `git-config-get-int64` (as `-int`)
- :grey_question: `git-config-get-mapped`
- :grey_question: `git-config-get-multivar-foreach`
- :heavy_check_mark: `git-config-get-path`
- :heavy_check_mark: `git-config-get-string`
- :x: `git-config-get-string-buf` (probably fine with just `-get-string`)
- :x: `git-config-init-backend` (in `sys`)
- :x: `git-config-iterator-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-config-iterator-glob-new`
- :grey_question: `git-config-iterator-new`
- :heavy_check_mark: `git-config-lock`
- :grey_question: `git-config-lookup-map-value`
- :grey_question: `git-config-multivar-iterator-new`
- :heavy_check_mark: `git-config-new`
- :grey_question: `git-config-next`
- :heavy_check_mark: `git-config-open-default`
- :heavy_check_mark: `git-config-open-global`
- :heavy_check_mark: `git-config-open-level`
- :heavy_check_mark: `git-config-open-ondisk`
- :grey_question: `git-config-parse-bool`
- :grey_question: `git-config-parse-int32`
- :grey_question: `git-config-parse-int64`
- :grey_question: `git-config-parse-path`
- :heavy_check_mark: `git-config-set-bool`
- :x: `git-config-set-int32` (don't need different integer types)
- :heavy_check_mark: `git-config-set-int64` (as `-int`)
- :grey_question: `git-config-set-multivar`
- :heavy_check_mark: `git-config-set-string`
- :heavy_check_mark: `git-config-snapshot`

### cred

- :heavy_check_mark: `git-cred-default-new`
- :x: `git-cred-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-cred-has-username` (`git-cred-username-p`)
- :grey_question: `git-cred-ssh-custom-new`
- :grey_question: `git-cred-ssh-interactive-new`
- :heavy_check_mark: `git-cred-ssh-key-from-agent`
- :heavy_check_mark: `git-cred-ssh-key-memory-new`
- :heavy_check_mark: `git-cred-ssh-key-new`
- :heavy_check_mark: `git-cred-username-new`
- :x: `git-cred-userpass` (stock callback)
- :heavy_check_mark: `git-cred-userpass-plaintext-new`

### describe

- :heavy_check_mark: `git-describe-commit`
- :x: `git-describe-format` (we return strings immediately)
- :x: `git-describe-result-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-describe-workdir`

### diff

- :grey_question: `git-diff-blob-to-buffer`
- :grey_question: `git-diff-blobs`
- :grey_question: `git-diff-buffers`
- :grey_question: `git-diff-commit-as-email`
- :x: `git-diff-find-init-options`
- :heavy_check_mark: `git-diff-find-similar`
- :heavy_check_mark: `git-diff-foreach`
- :grey_question: `git-diff-format-email`
- :grey_question: `git-diff-format-email-init-options`
- :x: `git-diff-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-diff-from-buffer`
- :heavy_check_mark: `git-diff-get-delta`
- :x: `git-diff-get-perfdata` (in `sys`)
- :grey_question: `git-diff-get-stats`
- :heavy_check_mark: `git-diff-index-to-index`
- :heavy_check_mark: `git-diff-index-to-workdir`
- :grey_question: `git-diff-init-options`
- :grey_question: `git-diff-is-sorted-icase`
- :grey_question: `git-diff-merge`
- :heavy_check_mark: `git-diff-num-deltas`
- :heavy_check_mark: `git-diff-num-deltas-of-type` (use `git-diff-num-deltas`)
- :grey_question: `git-diff-patchid`
- :grey_question: `git-diff-patchid-init-options`
- :heavy_check_mark: `git-diff-print`
- :x: `git-diff-print-callback--to-buf` (in `sys`)
- :grey_question: `git-diff-print-callback--to-file-handle`
- :grey_question: `git-diff-stats-deletions`
- :grey_question: `git-diff-stats-files-changed`
- :x: `git-diff-stats-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-diff-stats-insertions`
- :grey_question: `git-diff-stats-to-buf`
- :grey_question: `git-diff-status-char`
- :grey_question: `git-diff-to-buf`
- :heavy_check_mark: `git-diff-tree-to-index`
- :heavy_check_mark: `git-diff-tree-to-tree`
- :heavy_check_mark: `git-diff-tree-to-workdir`
- :heavy_check_mark: `git-diff-tree-to-workdir-with-index`

### fetch

- :x: `git-fetch-init-options`

### filter

- :x: `git-filter-init` (in `sys`)
- :grey_question: `git-filter-list-apply-to-blob`
- :grey_question: `git-filter-list-apply-to-data`
- :grey_question: `git-filter-list-apply-to-file`
- :grey_question: `git-filter-list-contains`
- :x: `git-filter-list-free` (memory management shouldn't be exposed to Emacs)
- :x: `git-filter-list-length` (in `sys`)
- :grey_question: `git-filter-list-load`
- :x: `git-filter-list-new` (in `sys`)
- :x: `git-filter-list-push` (in `sys`)
- :grey_question: `git-filter-list-stream-blob`
- :grey_question: `git-filter-list-stream-data`
- :grey_question: `git-filter-list-stream-file`
- :x: `git-filter-lookup` (in `sys`)
- :x: `git-filter-register` (in `sys`)
- :x: `git-filter-source-filemode` (in `sys`)
- :x: `git-filter-source-flags` (in `sys`)
- :x: `git-filter-source-id` (in `sys`)
- :x: `git-filter-source-mode` (in `sys`)
- :x: `git-filter-source-path` (in `sys`)
- :x: `git-filter-source-repo` (in `sys`)
- :x: `git-filter-unregister` (in `sys`)

### giterr

Probably none of these functions will be necessary, since we expose errors to Emacs as signals.

- :x: `giterr-clear`
- :x: `giterr-last`
- :x: `giterr-set-oom`
- :x: `giterr-set-str`

### graph

- :heavy_check_mark: `git-graph-ahead-behind`
- :heavy_check_mark: `git-graph-descendant-of`

### hashsig

- :x: `git-hashsig-compare` (in `sys`)
- :x: `git-hashsig-create` (in `sys`)
- :x: `git-hashsig-create-fromfile` (in `sys`)
- :x: `git-hashsig-free` (memory management shouldn't be exposed to Emacs)

### ignore

- :heavy_check_mark: `git-ignore-add-rule`
- :heavy_check_mark: `git-ignore-clear-internal-rules`
- :heavy_check_mark: `git-ignore-path-is-ignored`

### index

- :grey_question: `git-index-add`
- :heavy_check_mark: `git-index-add-all`
- :heavy_check_mark: `git-index-add-bypath`
- :grey_question: `git-index-add-frombuffer`
- :heavy_check_mark: `git-index-caps`
- :heavy_check_mark: `git-index-checksum`
- :heavy_check_mark: `git-index-clear`
- :grey_question: `git-index-conflict-add`
- :grey_question: `git-index-conflict-cleanup`
- :heavy_check_mark: `git-index-conflict-get`
- :heavy_check_mark: `git-index-conflict-iterator-free` (use `git-index-conflict-foreach`)
- :heavy_check_mark: `git-index-conflict-iterator-new` (use `git-index-conflict-foreach`)
- :heavy_check_mark: `git-index-conflict-next` (use `git-index-conflict-foreach`)
- :grey_question: `git-index-conflict-remove`
- :heavy_check_mark: `git-index-entry-is-conflict` (use `git-index-entry-stage`)
- :heavy_check_mark: `git-index-entry-stage`
- :heavy_check_mark: `git-index-entrycount`
- :grey_question: `git-index-find`
- :grey_question: `git-index-find-prefix`
- :x: `git-index-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-index-get-byindex`
- :heavy_check_mark: `git-index-get-bypath`
- :heavy_check_mark: `git-index-has-conflicts` (as `git-index-conflicts-p`)
- :grey_question: `git-index-new`
- :grey_question: `git-index-open`
- :heavy_check_mark: `git-index-owner`
- :heavy_check_mark: `git-index-path`
- :heavy_check_mark: `git-index-read`
- :grey_question: `git-index-read-tree`
- :grey_question: `git-index-remove`
- :grey_question: `git-index-remove-all`
- :grey_question: `git-index-remove-bypath`
- :grey_question: `git-index-remove-directory`
- :grey_question: `git-index-set-caps`
- :grey_question: `git-index-set-version`
- :grey_question: `git-index-update-all`
- :heavy_check_mark: `git-index-version`
- :heavy_check_mark: `git-index-write`
- :heavy_check_mark: `git-index-write-tree`
- :heavy_check_mark: `git-index-write-tree-to` (use `git-index-write-tree`)

### indexer

- :grey_question: `git-indexer-append`
- :grey_question: `git-indexer-commit`
- :x: `git-indexer-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-indexer-hash`
- :grey_question: `git-indexer-new`

### libgit2

- :heavy_check_mark: `git-libgit2-features` (as `libgit-feature-p`)
- :x: `git-libgit2-init` (internal)
- :grey_question: `git-libgit2-opts`
- :x: `git-libgit2-shutdown` (internal)
- :heavy_check_mark: `git-libgit2-version` (as `libgit-version`)

### mempack

- :x: `git-mempack-dump` (in `sys`)
- :x: `git-mempack-new` (in `sys`)
- :x: `git-mempack-reset` (in `sys`)

### merge

- :heavy_check_mark: `git-merge`
- :heavy_check_mark: `git-merge-analysis`
- :heavy_check_mark: `git-merge-base`
- :heavy_check_mark: `git-merge-base-many` (use `git-merge-base`)
- :heavy_check_mark: `git-merge-base-octopus`
- :heavy_check_mark: `git-merge-bases`
- :heavy_check_mark: `git-merge-bases-many` (use `git-merge-bases`)
- :grey_question: `git-merge-commits`
- :grey_question: `git-merge-file`
- :grey_question: `git-merge-file-from-index`
- :grey_question: `git-merge-file-init-input`
- :grey_question: `git-merge-file-init-options`
- :grey_question: `git-merge-file-result-free`
- :x: `git-merge-init-options`
- :grey_question: `git-merge-trees`

### message

- :heavy_check_mark: `git-message-prettify`
- :x: `git-message-trailer-array-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-message-trailers`

### note

- :grey_question: `git-note-author`
- :grey_question: `git-note-commit-create`
- :grey_question: `git-note-commit-iterator-new`
- :grey_question: `git-note-commit-read`
- :grey_question: `git-note-commit-remove`
- :grey_question: `git-note-committer`
- :grey_question: `git-note-create`
- :grey_question: `git-note-foreach`
- :x: `git-note-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-note-id`
- :grey_question: `git-note-iterator-free`
- :grey_question: `git-note-iterator-new`
- :grey_question: `git-note-message`
- :grey_question: `git-note-next`
- :grey_question: `git-note-read`
- :grey_question: `git-note-remove`

### object

- :x: `git-object--size` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-object-dup`
- :x: `git-object-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-object-id`
- :grey_question: `git-object-lookup`
- :heavy_check_mark: `git-object-lookup-bypath`
- :heavy_check_mark: `git-object-lookup-prefix`
- :heavy_check_mark: `git-object-owner`
- :grey_question: `git-object-peel`
- :heavy_check_mark: `git-object-short-id`
- :x: `git-object-string2type` (see below)
- :x: `git-object-type` (can be covered by a more general `git-typeof` for all opaque user pointers)
- :x: `git-object-type2string` (see above)
- :grey_question: `git-object-typeisloose`

### odb

- :grey_question: `git-odb-add-alternate`
- :grey_question: `git-odb-add-backend`
- :grey_question: `git-odb-add-disk-alternate`
- :grey_question: `git-odb-backend-loose`
- :grey_question: `git-odb-backend-one-pack`
- :grey_question: `git-odb-backend-pack`
- :grey_question: `git-odb-exists`
- :grey_question: `git-odb-exists-prefix`
- :grey_question: `git-odb-expand-ids`
- :grey_question: `git-odb-foreach`
- :x: `git-odb-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-odb-get-backend`
- :grey_question: `git-odb-hash`
- :grey_question: `git-odb-hashfile`
- :x: `git-odb-init-backend` (in `sys`)
- :grey_question: `git-odb-new`
- :grey_question: `git-odb-num-backends`
- :grey_question: `git-odb-object-data`
- :grey_question: `git-odb-object-dup`
- :x: `git-odb-object-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-odb-object-id`
- :grey_question: `git-odb-object-size`
- :grey_question: `git-odb-object-type`
- :grey_question: `git-odb-open`
- :grey_question: `git-odb-open-rstream`
- :grey_question: `git-odb-open-wstream`
- :grey_question: `git-odb-read`
- :grey_question: `git-odb-read-header`
- :grey_question: `git-odb-read-prefix`
- :grey_question: `git-odb-refresh`
- :grey_question: `git-odb-stream-finalize-write`
- :x: `git-odb-stream-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-odb-stream-read`
- :grey_question: `git-odb-stream-write`
- :grey_question: `git-odb-write`
- :grey_question: `git-odb-write-pack`

### oid

Probably none of these functions will be necessary, since we can expose OIDs to Emacs as strings.

- :x: `git-oid-cmp`
- :x: `git-oid-cpy`
- :x: `git-oid-equal`
- :x: `git-oid-fmt`
- :x: `git-oid-fromraw`
- :x: `git-oid-fromstr`
- :x: `git-oid-fromstrn`
- :x: `git-oid-fromstrp`
- :x: `git-oid-iszero`
- :x: `git-oid-ncmp`
- :x: `git-oid-nfmt`
- :x: `git-oid-pathfmt`
- :x: `git-oid-shorten-add`
- :x: `git-oid-shorten-free`
- :x: `git-oid-shorten-new`
- :x: `git-oid-strcmp`
- :x: `git-oid-streq`
- :x: `git-oid-tostr`
- :x: `git-oid-tostr-s`

### oidarray

- :x: `git-oidarray-free` (memory management shouldn't be exposed to Emacs)

### openssl

- :x: `git-openssl-set-locking` (in `sys`)

### packbuilder

- :grey_question: `git-packbuilder-foreach`
- :x: `git-packbuilder-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-packbuilder-hash`
- :grey_question: `git-packbuilder-insert`
- :grey_question: `git-packbuilder-insert-commit`
- :grey_question: `git-packbuilder-insert-recur`
- :grey_question: `git-packbuilder-insert-tree`
- :grey_question: `git-packbuilder-insert-walk`
- :grey_question: `git-packbuilder-new`
- :grey_question: `git-packbuilder-object-count`
- :grey_question: `git-packbuilder-set-callbacks`
- :grey_question: `git-packbuilder-set-threads`
- :grey_question: `git-packbuilder-write`
- :grey_question: `git-packbuilder-written`

### patch

- :x: `git-patch-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-patch-from-blob-and-buffer`
- :grey_question: `git-patch-from-blobs`
- :grey_question: `git-patch-from-buffers`
- :grey_question: `git-patch-from-diff`
- :grey_question: `git-patch-get-delta`
- :grey_question: `git-patch-get-hunk`
- :grey_question: `git-patch-get-line-in-hunk`
- :grey_question: `git-patch-line-stats`
- :grey_question: `git-patch-num-hunks`
- :grey_question: `git-patch-num-lines-in-hunk`
- :grey_question: `git-patch-print`
- :grey_question: `git-patch-size`
- :grey_question: `git-patch-to-buf`

### pathspec

- :x: `git-pathspec-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-pathspec-match-diff`
- :heavy_check_mark: `git-pathspec-match-index`
- :heavy_check_mark: `git-pathspec-match-list-diff-entry`
- :heavy_check_mark: `git-pathspec-match-list-entry`
- :heavy_check_mark: `git-pathspec-match-list-entrycount`
- :heavy_check_mark: `git-pathspec-match-list-failed-entry`
- :heavy_check_mark: `git-pathspec-match-list-failed-entrycount`
- :x: `git-pathspec-match-list-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-pathspec-match-tree`
- :heavy_check_mark: `git-pathspec-match-workdir`
- :heavy_check_mark: `git-pathspec-matches-path`
- :heavy_check_mark: `git-pathspec-new`

### proxy

- :x: `git-proxy-init-options`

### push

- :x: `git-push-init-options`

### rebase

- :grey_question: `git-rebase-abort`
- :grey_question: `git-rebase-commit`
- :grey_question: `git-rebase-finish`
- :x: `git-rebase-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-rebase-init`
- :grey_question: `git-rebase-init-options`
- :grey_question: `git-rebase-inmemory-index`
- :grey_question: `git-rebase-next`
- :grey_question: `git-rebase-open`
- :grey_question: `git-rebase-operation-byindex`
- :grey_question: `git-rebase-operation-current`
- :grey_question: `git-rebase-operation-entrycount`

### refdb

- :x: `git-refdb-backend-fs` (in `sys`)
- :grey_question: `git-refdb-compress`
- :x: `git-refdb-free` (memory management shouldn't be exposed to Emacs)
- :x: `git-refdb-init-backend` (in `sys`)
- :grey_question: `git-refdb-new`
- :grey_question: `git-refdb-open`
- :x: `git-refdb-set-backend` (in `sys`)

### reference

- :x: `git-reference--alloc` (in `sys`)
- :x: `git-reference--alloc-symbolic` (in `sys`)
- :grey_question: `git-reference-cmp`
- :heavy_check_mark: `git-reference-create`
- :heavy_check_mark: `git-reference-create-matching`
- :heavy_check_mark: `git-reference-delete`
- :heavy_check_mark: `git-reference-dup`
- :heavy_check_mark: `git-reference-dwim`
- :heavy_check_mark: `git-reference-ensure-log`
- :heavy_check_mark: `git-reference-foreach`
- :heavy_check_mark: `git-reference-foreach-glob`
- :heavy_check_mark: `git-reference-foreach-name`
- :x: `git-reference-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-reference-has-log`
- :heavy_check_mark: `git-reference-is-branch`
- :heavy_check_mark: `git-reference-is-note`
- :heavy_check_mark: `git-reference-is-remote`
- :heavy_check_mark: `git-reference-is-tag`
- :heavy_check_mark: `git-reference-is-valid-name`
- :x: `git-reference-iterator-free` (use the foreach functions)
- :x: `git-reference-iterator-glob-new` (use the foreach functions)
- :x: `git-reference-iterator-new` (use the foreach functions)
- :heavy_check_mark: `git-reference-list`
- :heavy_check_mark: `git-reference-lookup`
- :heavy_check_mark: `git-reference-name`
- :heavy_check_mark: `git-reference-name-to-id`
- :x: `git-reference-next` (use the foreach functions)
- :x: `git-reference-next-name` (use the foreach functions)
- :grey_question: `git-reference-normalize-name`
- :heavy_check_mark: `git-reference-owner`
- :heavy_check_mark: `git-reference-peel`
- :heavy_check_mark: `git-reference-remove`
- :grey_question: `git-reference-rename`
- :heavy_check_mark: `git-reference-resolve`
- :grey_question: `git-reference-set-target`
- :heavy_check_mark: `git-reference-shorthand`
- :grey_question: `git-reference-symbolic-create`
- :grey_question: `git-reference-symbolic-create-matching`
- :grey_question: `git-reference-symbolic-set-target`
- :heavy_check_mark: `git-reference-symbolic-target`
- :heavy_check_mark: `git-reference-target`
- :heavy_check_mark: `git-reference-target-peel`
- :heavy_check_mark: `git-reference-type`

### reflog

- :heavy_check_mark: `git-reflog-append`
- :heavy_check_mark: `git-reflog-delete`
- :heavy_check_mark: `git-reflog-drop`
- :heavy_check_mark: `git-reflog-entry-byindex`
- :heavy_check_mark: `git-reflog-entry-committer`
- :heavy_check_mark: `git-reflog-entry-id-new` (use `git-reflog-entry-id`)
- :heavy_check_mark: `git-reflog-entry-id-old` (use `git-reflog-entry-id`)
- :heavy_check_mark: `git-reflog-entry-message`
- :heavy_check_mark: `git-reflog-entrycount`
- :x: `git-reflog-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-reflog-read`
- :heavy_check_mark: `git-reflog-rename`
- :heavy_check_mark: `git-reflog-write`

### refspec

- :heavy_check_mark: `git-refspec-direction`
- :heavy_check_mark: `git-refspec-dst`
- :heavy_check_mark: `git-refspec-dst-matches`
- :heavy_check_mark: `git-refspec-force`
- :grey_question: `git-refspec-rtransform`
- :heavy_check_mark: `git-refspec-src`
- :heavy_check_mark: `git-refspec-src-matches`
- :heavy_check_mark: `git-refspec-string`
- :grey_question: `git-refspec-transform`

### remote

- :heavy_check_mark: `git-remote-add-fetch` (use `git-remote-add-refspec`)
- :heavy_check_mark: `git-remote-add-push` (use `git-remote-add-refspec`)
- :heavy_check_mark: `git-remote-autotag`
- :grey_question: `git-remote-connect`
- :grey_question: `git-remote-connected`
- :heavy_check_mark: `git-remote-create`
- :grey_question: `git-remote-create-anonymous`
- :grey_question: `git-remote-create-detached`
- :grey_question: `git-remote-create-with-fetchspec`
- :grey_question: `git-remote-default-branch`
- :grey_question: `git-remote-delete`
- :grey_question: `git-remote-disconnect`
- :grey_question: `git-remote-download`
- :grey_question: `git-remote-dup`
- :heavy_check_mark: `git-remote-fetch`
- :x: `git-remote-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-remote-get-fetch-refspecs` (use `git-remote-get-refspecs`)
- :heavy_check_mark: `git-remote-get-push-refspecs` (use `git-remote-get-refspecs`)
- :heavy_check_mark: `git-remote-get-refspec`
- :x: `git-remote-init-callbacks`
- :heavy_check_mark: `git-remote-is-valid-name`
- :heavy_check_mark: `git-remote-list`
- :heavy_check_mark: `git-remote-lookup`
- :grey_question: `git-remote-ls`
- :heavy_check_mark: `git-remote-name`
- :heavy_check_mark: `git-remote-owner`
- :grey_question: `git-remote-prune`
- :grey_question: `git-remote-prune-refs`
- :heavy_check_mark: `git-remote-push`
- :heavy_check_mark: `git-remote-pushurl`
- :heavy_check_mark: `git-remote-refspec-count`
- :grey_question: `git-remote-rename`
- :grey_question: `git-remote-set-autotag`
- :grey_question: `git-remote-set-pushurl`
- :grey_question: `git-remote-set-url`
- :grey_question: `git-remote-stats`
- :grey_question: `git-remote-stop`
- :grey_question: `git-remote-update-tips`
- :grey_question: `git-remote-upload`
- :heavy_check_mark: `git-remote-url`

### repository

- :x: `git-repository--cleanup` (in `sys`)
- :heavy_check_mark: `git-repository-commondir`
- :heavy_check_mark: `git-repository-config`
- :grey_question: `git-repository-config-snapshot`
- :heavy_check_mark: `git-repository-detach-head`
- :heavy_check_mark: `git-repository-discover`
- :grey_question: `git-repository-fetchhead-foreach`
- :x: `git-repository-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-repository-get-namespace`
- :grey_question: `git-repository-hashfile`
- :heavy_check_mark: `git-repository-head`
- :heavy_check_mark: `git-repository-head-detached`
- :heavy_check_mark: `git-repository-head-for-worktree`
- :heavy_check_mark: `git-repository-head-unborn`
- :heavy_check_mark: `git-repository-ident`
- :heavy_check_mark: `git-repository-index`
- :heavy_check_mark: `git-repository-init`
- :grey_question: `git-repository-init-ext`
- :grey_question: `git-repository-init-init-options`
- :heavy_check_mark: `git-repository-is-bare`
- :heavy_check_mark: `git-repository-is-empty`
- :heavy_check_mark: `git-repository-is-shallow`
- :heavy_check_mark: `git-repository-is-worktree`
- :grey_question: `git-repository-item-path`
- :grey_question: `git-repository-mergehead-foreach`
- :heavy_check_mark: `git-repository-message`
- :heavy_check_mark: `git-repository-message-remove`
- :x: `git-repository-new` (in `sys`)
- :grey_question: `git-repository-odb`
- :heavy_check_mark: `git-repository-open`
- :heavy_check_mark: `git-repository-open-bare`
- :grey_question: `git-repository-open-ext`
- :grey_question: `git-repository-open-from-worktree`
- :heavy_check_mark: `git-repository-path`
- :grey_question: `git-repository-refdb`
- :x: `git-repository-reinit-filesystem` (in `sys`)
- :x: `git-repository-set-bare` (in `sys`)
- :x: `git-repository-set-config` (in `sys`)
- :heavy_check_mark: `git-repository-set-head`
- :heavy_check_mark: `git-repository-set-head-detached`
- :grey_question: `git-repository-set-head-detached-from-annotated`
- :heavy_check_mark: `git-repository-set-ident`
- :x: `git-repository-set-index` (in `sys`)
- :heavy_check_mark: `git-repository-set-namespace`
- :x: `git-repository-set-odb` (in `sys`)
- :x: `git-repository-set-refdb` (in `sys`)
- :heavy_check_mark: `git-repository-set-workdir`
- :heavy_check_mark: `git-repository-state`
- :heavy_check_mark: `git-repository-state-cleanup`
- :x: `git-repository-submodule-cache-all` (in `sys`)
- :x: `git-repository-submodule-cache-clear` (in `sys`)
- :heavy_check_mark: `git-repository-workdir`
- :grey_question: `git-repository-wrap-odb`

### reset

- :heavy_check_mark: `git-reset`
- :heavy_check_mark: `git-reset-default`
- :heavy_check_mark: `git-reset-from-annotated`

### revert

- :heavy_check_mark: `git-revert`
- :heavy_check_mark: `git-revert-commit`
- :x: `git-revert-init-options`

### revparse

- :heavy_check_mark: `git-revparse`
- :heavy_check_mark: `git-revparse-ext`
- :heavy_check_mark: `git-revparse-single`

### revwalk

- :x: `git-revwalk-add-hide-cb` (use `git-revwalk-foreach`)
- :x: `git-revwalk-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-revwalk-hide`
- :heavy_check_mark: `git-revwalk-hide-glob`
- :heavy_check_mark: `git-revwalk-hide-head`
- :heavy_check_mark: `git-revwalk-hide-ref`
- :heavy_check_mark: `git-revwalk-new`
- :x: `git-revwalk-next` (use `git-revwalk-foreach`)
- :heavy_check_mark: `git-revwalk-push`
- :heavy_check_mark: `git-revwalk-push-glob`
- :heavy_check_mark: `git-revwalk-push-head`
- :heavy_check_mark: `git-revwalk-push-range`
- :heavy_check_mark: `git-revwalk-push-ref`
- :heavy_check_mark: `git-revwalk-repository`
- :heavy_check_mark: `git-revwalk-reset`
- :heavy_check_mark: `git-revwalk-simplify-first-parent`
- :heavy_check_mark: `git-revwalk-sorting`

### signature

- :heavy_check_mark: `git-signature-default`
- :grey_question: `git-signature-dup`
- :x: `git-signature-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-signature-from-buffer` (use `git-signature-from-string`)
- :heavy_check_mark: `git-signature-new`
- :heavy_check_mark: `git-signature-now`

### smart

- :grey_question: `git-smart-subtransport-git`
- :grey_question: `git-smart-subtransport-http`
- :grey_question: `git-smart-subtransport-ssh`

### stash

- :grey_question: `git-stash-apply`
- :grey_question: `git-stash-apply-init-options`
- :grey_question: `git-stash-drop`
- :grey_question: `git-stash-foreach`
- :grey_question: `git-stash-pop`

### status

- :grey_question: `git-status-byindex`
- :heavy_check_mark: `git-status-file`
- :heavy_check_mark: `git-status-foreach`
- :heavy_check_mark: `git-status-foreach-ext`
- :x: `git-status-init-options`
- :grey_question: `git-status-list-entrycount`
- :grey_question: `git-status-list-free`
- :x: `git-status-list-get-perfdata` (in `sys`)
- :grey_question: `git-status-list-new`
- :heavy_check_mark: `git-status-should-ignore`

### strarray

- :x: `git-strarray-copy`
- :x: `git-strarray-free` (memory management shouldn't be exposed to Emacs)

### stream

- :x: `git-stream-register-tls` (in `sys`)

### submodule

- :heavy_check_mark: `git-submodule-add-finalize`
- :heavy_check_mark: `git-submodule-add-setup`
- :heavy_check_mark: `git-submodule-add-to-index`
- :heavy_check_mark: `git-submodule-branch`
- :heavy_check_mark: `git-submodule-fetch-recurse-submodules`
- :heavy_check_mark: `git-submodule-foreach`
- :x: `git-submodule-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-submodule-head-id`
- :heavy_check_mark: `git-submodule-ignore`
- :heavy_check_mark: `git-submodule-index-id`
- :heavy_check_mark: `git-submodule-init`
- :heavy_check_mark: `git-submodule-location`
- :heavy_check_mark: `git-submodule-lookup`
- :heavy_check_mark: `git-submodule-name`
- :heavy_check_mark: `git-submodule-open`
- :heavy_check_mark: `git-submodule-owner`
- :heavy_check_mark: `git-submodule-path`
- :heavy_check_mark: `git-submodule-reload`
- :heavy_check_mark: `git-submodule-repo-init`
- :grey_question: `git-submodule-resolve-url`
- :heavy_check_mark: `git-submodule-set-branch`
- :heavy_check_mark: `git-submodule-set-fetch-recurse-submodules`
- :heavy_check_mark: `git-submodule-set-ignore`
- :heavy_check_mark: `git-submodule-set-update`
- :heavy_check_mark: `git-submodule-set-url`
- :heavy_check_mark: `git-submodule-status`
- :heavy_check_mark: `git-submodule-sync`
- :heavy_check_mark: `git-submodule-update`
- :x: `git-submodule-update-init-options`
- :heavy_check_mark: `git-submodule-update-strategy`
- :heavy_check_mark: `git-submodule-url`
- :heavy_check_mark: `git-submodule-wd-id`

### tag

- :grey_question: `git-tag-annotation-create`
- :grey_question: `git-tag-create`
- :grey_question: `git-tag-create-frombuffer`
- :grey_question: `git-tag-create-lightweight`
- :grey_question: `git-tag-delete`
- :grey_question: `git-tag-dup`
- :heavy_check_mark: `git-tag-foreach`
- :x: `git-tag-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-tag-id`
- :heavy_check_mark: `git-tag-list`
- :heavy_check_mark: `git-tag-list-match`
- :heavy_check_mark: `git-tag-lookup`
- :heavy_check_mark: `git-tag-lookup-prefix`
- :heavy_check_mark: `git-tag-message`
- :heavy_check_mark: `git-tag-name`
- :heavy_check_mark: `git-tag-owner`
- :heavy_check_mark: `git-tag-peel`
- :heavy_check_mark: `git-tag-tagger`
- :heavy_check_mark: `git-tag-target`
- :heavy_check_mark: `git-tag-target-id`
- :heavy_check_mark: `git-tag-target-type`

### time

- :x: `git-time-monotonic` (in `sys`)

### trace

- :grey_question: `git-trace-set`

### transaction

- :heavy_check_mark: `git_transaction_commit`
- :x: `git_transaction_free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git_transaction_lock_ref`
- :grey_question: `git_transaction_new`
- :grey_question: `git_transaction_remove`
- :grey_question: `git_transaction_set_reflog`
- :grey_question: `git_transaction_set_symbolic_target`
- :grey_question: `git_transaction_set_target`

### transport

- :x: `git-transport-dummy` (in `sys`)
- :x: `git-transport-init` (in `sys`)
- :x: `git-transport-local` (in `sys`)
- :x: `git-transport-new` (in `sys`)
- :x: `git-transport-register` (in `sys`)
- :x: `git-transport-smart` (in `sys`)
- :x: `git-transport-smart-certificate-check` (in `sys`)
- :x: `git-transport-smart-credentials` (in `sys`)
- :x: `git-transport-smart-proxy-options` (in `sys`)
- :x: `git-transport-ssh-with-paths` (in `sys`)
- :x: `git-transport-unregister` (in `sys`)

### tree

- :grey_question: `git-tree-create-updated`
- :grey_question: `git-tree-dup`
- :heavy_check_mark: `git-tree-entry-byid`
- :heavy_check_mark: `git-tree-entry-byindex`
- :heavy_check_mark: `git-tree-entry-byname`
- :heavy_check_mark: `git-tree-entry-bypath`
- :x: `git-tree-entry-cmp` (tree entries are exposed to emacs as lists)
- :x: `git-tree-entry-dup` (tree entries are exposed to emacs as lists)
- :x: `git-tree-entry-filemode` (tree entries are exposed to emacs as lists)
- :x: `git-tree-entry-filemode-raw` (tree entries are exposed to emacs as lists)
- :x: `git-tree-entry-free` (memory management shouldn't be exposed to Emacs)
- :x: `git-tree-entry-id` (tree entries are exposed to emacs as lists)
- :x: `git-tree-entry-name` (tree entries are exposed to emacs as lists)
- :x: `git-tree-entry-to-object` (tree entries are exposed to emacs as lists)
- :x: `git-tree-entry-type` (tree entries are exposed to emacs as lists)
- :heavy_check_mark: `git-tree-entrycount`
- :x: `git-tree-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-tree-id`
- :heavy_check_mark: `git-tree-lookup`
- :heavy_check_mark: `git-tree-lookup-prefix`
- :heavy_check_mark: `git-tree-owner`
- :heavy_check_mark: `git-tree-walk`

### treebuilder

- :heavy_check_mark: `git-treebuilder-clear`
- :heavy_check_mark: `git-treebuilder-entrycount`
- :heavy_check_mark: `git-treebuilder-filter`
- :x: `git-treebuilder-free` (memory management shouldn't be exposed to Emacs)
- :heavy_check_mark: `git-treebuilder-get`
- :heavy_check_mark: `git-treebuilder-insert`
- :heavy_check_mark: `git-treebuilder-new`
- :heavy_check_mark: `git-treebuilder-remove`
- :heavy_check_mark: `git-treebuilder-write`
- :grey_question: `git-treebuilder-write-with-buffer`

### worktree

- :grey_question: `git-worktree-add`
- :grey_question: `git-worktree-add-init-options`
- :x: `git-worktree-free` (memory management shouldn't be exposed to Emacs)
- :grey_question: `git-worktree-is-locked`
- :grey_question: `git-worktree-is-prunable`
- :grey_question: `git-worktree-list`
- :grey_question: `git-worktree-lock`
- :grey_question: `git-worktree-lookup`
- :grey_question: `git-worktree-open-from-repository`
- :grey_question: `git-worktree-prune`
- :grey_question: `git-worktree-prune-init-options`
- :grey_question: `git-worktree-unlock`
- :grey_question: `git-worktree-validate`
