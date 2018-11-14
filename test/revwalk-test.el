(ert-deftest revwalk-linear ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (commit-change "b" "abc")
    (commit-change "c" "abc")
    (commit-change "d" "abc")
    (commit-change "e" "abc")
    (commit-change "f" "abc")
    (let* ((repo (libgit-repository-open path))
           (walk (libgit-revwalk-new repo))
           (head (libgit-revparse-single repo "HEAD"))
           seen)
      (libgit-revwalk-push-head walk)
      (libgit-revwalk-foreach walk (lambda (id) (push id seen)))
      (should (equal (reverse seen)
                     (cl-loop for i below 6
                              collect (libgit-commit-id
                                       (libgit-commit-nth-gen-ancestor head i))))))))

(ert-deftest revwalk-hidden ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (commit-change "b" "abc")
    (commit-change "c" "abc")
    (commit-change "d" "abc")
    (commit-change "e" "abc")
    (commit-change "f" "abc")
    (let* ((repo (libgit-repository-open path))
           (walk (libgit-revwalk-new repo))
           (head (libgit-revparse-single repo "HEAD"))
           seen)
      (libgit-revwalk-push-head walk)
      (libgit-revwalk-hide walk (libgit-commit-id
                                 (libgit-commit-nth-gen-ancestor head 3)))
      (libgit-revwalk-foreach walk (lambda (id) (push id seen)))
      (should (equal (reverse seen)
                     (cl-loop for i below 3
                              collect (libgit-commit-id
                                       (libgit-commit-nth-gen-ancestor head i))))))))

(ert-deftest revwalk-range ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (commit-change "b" "abc")
    (commit-change "c" "abc")
    (commit-change "d" "abc")
    (commit-change "e" "abc")
    (commit-change "f" "abc")
    (let* ((repo (libgit-repository-open path))
           (walk (libgit-revwalk-new repo))
           (head (libgit-revparse-single repo "HEAD"))
           seen)
      (libgit-revwalk-push-range
       walk
       (concat (libgit-commit-id (libgit-commit-nth-gen-ancestor head 3))
               ".." (libgit-commit-id head)))
      (libgit-revwalk-foreach walk (lambda (id) (push id seen)))
      (should (equal (reverse seen)
                     (cl-loop for i below 3
                              collect (libgit-commit-id
                                       (libgit-commit-nth-gen-ancestor head i))))))))

(ert-deftest revwalk-hide-callback ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (commit-change "b" "abc")
    (commit-change "c" "abc")
    (commit-change "d" "abc")
    (commit-change "e" "abc")
    (commit-change "f" "abc")
    (let* ((repo (libgit-repository-open path))
           (walk (libgit-revwalk-new repo))
           (head (libgit-revparse-single repo "HEAD"))
           seen)
      (libgit-revwalk-push-head walk)
      (libgit-revwalk-foreach
       walk
       (lambda (id) (push id seen))
       (lambda (id)
         (string= id (libgit-commit-id
                      (libgit-commit-nth-gen-ancestor head 3)))))
      (should (equal (reverse seen)
                     (cl-loop for i below 3
                              collect (libgit-commit-id
                                       (libgit-commit-nth-gen-ancestor head i))))))))
