(ert-deftest refcount ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let ((repo (libgit-repository-open path)))
      (should (= 1 (libgit--refcount repo)))
      (let ((ref (libgit-repository-head repo)))
        (should (= 2 (libgit--refcount repo)))
        (let ((obj (libgit-reference-peel ref)))
          (should (= 3 (libgit--refcount repo))))))))

(ert-deftest memtest-1 ()
  (skip-unless nil)

  ;; Clean out stacks
  (garbage-collect)
  (libgit--allocs)
  (libgit--finalizes)
  (libgit--frees)

  (let (repo repo-ptr ref ref-ptr obj obj-ptr new-repo)
    (with-temp-dir path
      (init)
      (commit-change "test" "content")

      ;; Allocate exactly one repository object
      (setq repo (libgit-repository-open path))
      (setq repo-ptr (libgit--wrapper repo))
      (should (member repo-ptr (libgit--allocs)))
      (should-not (member repo-ptr (libgit--finalizes)))
      (should-not (member repo-ptr (libgit--frees)))
      (should (= 0 (libgit--parent-wrapper repo)))

      ;; Allocate exactly one reference object
      (setq ref (libgit-repository-head repo))
      (setq ref-ptr (libgit--wrapper ref))
      (should (member ref-ptr (libgit--allocs)))
      (let ((fin (libgit--finalizes)))
        (should-not (member repo-ptr fin))
        (should-not (member ref-ptr fin)))
      (let ((free (libgit--frees)))
        (should-not (member repo-ptr free))
        (should-not (member ref-ptr free)))
      (should (= 2 (libgit--refcount repo)))
      (should (= repo-ptr (libgit--parent-wrapper ref)))

      ;; Allocate exactly one commit
      (setq obj (libgit-reference-peel ref))
      (setq obj-ptr (libgit--wrapper obj))
      (should (member obj-ptr (libgit--allocs)))
      (let ((fin (libgit--finalizes)))
        (should-not (member repo-ptr fin))
        (should-not (member ref-ptr fin)))
      (let ((free (libgit--frees)))
        (should-not (member repo-ptr free))
        (should-not (member ref-ptr free)))
      (should (= 3 (libgit--refcount repo)))
      (should (= repo-ptr (libgit--parent-wrapper obj)))

      ;; Delete the reference object and run GC
      ;; finalize and free the reference ptr
      ;; finalize but don't free the repo ptr
      (setq ref nil)
      (garbage-collect)
      (let ((fin (libgit--finalizes)))
        (should (member ref-ptr fin))
        (should (member repo-ptr fin)))
      (let ((free (libgit--frees)))
        (should (member ref-ptr free))
        (should-not (member repo-ptr free)))
      (should (= 2 (libgit--refcount repo)))

      ;; Get a new user-ptr to the repo
      ;; it's different in Emacs eq-sense but points
      ;; not only to the same git object but the same wrapper
      (setq new-repo (libgit-object-owner obj))
      (should-not (eq new-repo repo))
      (should (= repo-ptr (libgit--wrapper new-repo)))
      (should (= 3 (libgit--refcount repo))))))

(ert-deftest memtest-2 ()
  (skip-unless nil)

  ;; Clean out stacks
  (garbage-collect)
  (libgit--allocs)
  (libgit--finalizes)
  (libgit--frees)

  (let (repo repo-ptr ref ref-ptr)
    (with-temp-dir path
      (init)
      (commit-change "test" "content")

      ;; Allocate exactly one repository object
      (setq repo (libgit-repository-open path))
      (setq repo-ptr (libgit--wrapper repo))
      (should (member repo-ptr (libgit--allocs)))
      (should-not (member repo-ptr (libgit--finalizes)))
      (should-not (member repo-ptr (libgit--frees)))
      (should (= 0 (libgit--parent-wrapper repo)))

      ;; Allocate exactly one reference object
      (setq ref (libgit-repository-head repo))
      (setq ref-ptr (libgit--wrapper ref))
      (should (member ref-ptr (libgit--allocs)))
      (let ((fin (libgit--finalizes)))
        (should-not (member repo-ptr fin))
        (should-not (member ref-ptr fin)))
      (let ((free (libgit--frees)))
        (should-not (member repo-ptr free))
        (should-not (member ref-ptr free)))
      (should (= 2 (libgit--refcount repo)))
      (should (= repo-ptr (libgit--parent-wrapper ref)))

      ;; Delete the repository object and run GC
      (setq repo nil)
      (garbage-collect)
      (let ((fin (libgit--finalizes)))
        (should (member repo-ptr fin))
        (should-not (member ref-ptr fin)))
      (let ((free (libgit--frees)))
        (should-not (member repo-ptr free))
        (should-not (member ref-ptr free)))

      ;; Delete the reference object and run GC
      (setq ref nil)
      (garbage-collect)
      (let ((fin (libgit--finalizes)))
        (should (member repo-ptr fin))
        (should (member ref-ptr fin)))
      (let ((free (libgit--frees)))
        (should (member repo-ptr free))
        (should (member ref-ptr free))))))
