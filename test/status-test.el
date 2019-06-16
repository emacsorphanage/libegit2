(defun foreach-collect (repo &optional show flags pathspec baseline)
  (let (res)
    (libgit-status-foreach-ext
     repo
     (lambda (path status)
       (push (cons path (sort (libgit-status-decode status)
                              (lambda (a b) (string< (symbol-name a)
                                                     (symbol-name b)))))
             res))
     show flags pathspec baseline)
    (sort res (lambda (a b) (string< (car a) (car b))))))

(ert-deftest status-file ()
  (with-temp-dir path
    (init)
    (let ((repo (libgit-repository-open path)))
      (should-error (libgit-status-file repo "a") :type 'giterr-invalid)

      (write "a" "a")
      (should (equal '(wt-new) (libgit-status-file repo "a")))

      (run "git" "add" "a")
      (should (equal '(index-new) (libgit-status-file repo "a")))

      (write "a" "b")
      (should (equal '(index-new wt-modified) (libgit-status-file repo "a")))

      (commit)
      (should (equal '(wt-modified) (libgit-status-file repo "a")))

      (run "git" "checkout" "--" "a")
      (should (equal nil (libgit-status-file repo "a")))

      (delete-file "a")
      (should (equal '(wt-deleted) (libgit-status-file repo "a")))

      (run "git" "add" "a")
      (should (equal '(index-deleted) (libgit-status-file repo "a")))

      (write "b" "x")
      (write ".gitignore" "b")
      (should (equal '(ignored) (libgit-status-file repo "b")))

      (unless (memq system-type '(windows-nt ms-dos))
        (run "git" "reset" "HEAD" "a")
        (run "git" "checkout" "--" "a")
        (make-symbolic-link "b" "a" t)
        (should (equal '(wt-typechange) (libgit-status-file repo "a")))

        (run "git" "add" "a")
        (should (equal '(index-typechange) (libgit-status-file repo "a")))))))

(ert-deftest status-foreach ()
  (with-temp-dir path
    (init)
    (let ((repo (libgit-repository-open path)))

      (should (equal (foreach-collect repo) nil))
      (write "a" "a")
      (run "git" "add" "a")

      (should (equal (foreach-collect repo) '(("a" . (index-new)))))
      (write "a" "b")

      (should (equal (foreach-collect repo) '(("a" . (index-new wt-modified)))))
      (should (equal (foreach-collect repo 'index-only)
                     '(("a" . (index-new)))))

      (should (equal (foreach-collect repo 'workdir-only)
                     '(("a" . (wt-modified)))))
      (commit)

      (run "git" "checkout" "--" "a")

      (should (equal (foreach-collect repo nil '(include-unmodified))
                     '(("a" . nil))))
      (write "d/1" "")
      (write "d/2" "")

      (should (equal (foreach-collect repo nil '(include-untracked))
                     '(("d/" . (wt-new)))))

      (should (equal
               (foreach-collect
                repo nil '(include-untracked recurse-untracked-dirs))
               '(("d/1" . (wt-new)) ("d/2" . (wt-new)))))

      (delete-directory "d" t)
      (write "b" "b")

      (should (equal (foreach-collect repo nil '(include-untracked))
                     '(("b" . (wt-new)))))
      (write ".gitignore" "/b")

      (should (equal (foreach-collect repo nil '(include-ignored))
                     '(("b" . (ignored)))))
      (delete-file "b")

      (delete-file ".gitignore")
      (write "d/foo" "")
      (write "d/bar" "")
      (write "d/baz" "")
      (should (equal
               (foreach-collect
                repo nil '(include-untracked recurse-untracked-dirs)
                '("d/f*"))
               '(("d/foo" . (wt-new)))))

      (should (equal
               (foreach-collect
                repo nil '(include-untracked recurse-untracked-dirs)
                '("d/f*" "d/*z"))
               '(("d/baz" . (wt-new)) ("d/foo" . (wt-new)))))

      (should-error (libgit-status-foreach-ext repo nil) :type 'wrong-type-argument)
      (let ((i 0))
        (define-error 'foo "Foo")
        ;; Nonlocal exit test
        (should-error (libgit-status-foreach-ext
                       repo
                       (lambda (&rest args)
                         (when (= (cl-incf i) 2)
                           (signal 'foo "f")))
		       nil
		       '(include-untracked include-ignored include-unmodified))
		       :type 'foo)
        (should (= i 2)))
      (should-error (libgit-status-foreach-ext repo #'ignore 'foo)
                    :type 'wrong-value-argument)
      (should-error (libgit-status-foreach-ext repo #'ignore nil 1)
                    :type 'wrong-type-argument)
      (should-error (libgit-status-foreach-ext repo #'ignore nil '(foo))
                    :type 'wrong-value-argument)
      (should-error (libgit-status-foreach-ext repo #'ignore nil nil 1)
                    :type 'wrong-type-argument)
      (should-error (libgit-status-foreach-ext repo #'ignore nil nil '(foo))
                    :type 'wrong-type-argument)
      (should-error (libgit-status-foreach-ext repo #'ignore nil nil nil 1)
                    :type 'wrong-type-argument)
      (should-error (libgit-status-foreach-ext repo #'ignore nil
                                           '(no-refresh update-index))
                    :type 'giterr-invalid)

      ;; Should not error
      (libgit-status-foreach-ext
       repo #'ignore nil nil nil
       (libgit-reference-peel (libgit-repository-head repo) 'tree)))))

(ert-deftest status-should-ignore-p ()
  (with-temp-dir path
    (init)
    (write ".gitignore" "/foo/baz")
    (let ((repo (libgit-repository-open path)))
      (should (libgit-status-should-ignore-p repo "foo/baz"))
      (should (not (libgit-status-should-ignore-p repo "bar"))))))
