(ert-deftest tag-lookup ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (run "git" "tag" "-a" "first" "-m" "msg1")
    (commit-change "b" "def")
    (run "git" "tag" "-a" "second" "-m" "msg2")
    (let* ((repo (libgit-repository-open path))
           (tagid-1 (read-file-nnl ".git/refs/tags/first"))
           (tagid-2 (read-file-nnl ".git/refs/tags/second"))
           (tag1 (libgit-tag-lookup repo tagid-1))
           (tag2 (libgit-tag-lookup-prefix repo (substring tagid-2 0 7))))
      (should (libgit-tag-p tag1))
      (should (libgit-tag-p tag2))
      (should (string= (libgit-tag-id tag1) tagid-1))
      (should (string= (libgit-tag-id tag2) tagid-2))
      (should (string= (libgit-tag-name tag1) "first"))
      (should (string= (libgit-tag-name tag2) "second"))
      (should (string= (libgit-tag-message tag1) "msg1\n"))
      (should (string= (libgit-tag-message tag2) "msg2\n")))))

(ert-deftest tag-target ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (run "git" "tag" "-a" "commit" "-m" "z" "HEAD")
    (run "git" "tag" "-a" "tree" "-m" "z" "HEAD^{tree}")
    (let* ((repo (libgit-repository-open path))
           (tagid-1 (read-file-nnl ".git/refs/tags/commit"))
           (tagid-2 (read-file-nnl ".git/refs/tags/tree"))
           (tag1 (libgit-tag-lookup repo tagid-1))
           (tag2 (libgit-tag-lookup-prefix repo (substring tagid-2 0 7))))
      (should (eq 'commit (libgit-tag-target-type tag1)))
      (should (eq 'tree (libgit-tag-target-type tag2)))
      (should (string= (libgit-tag-target-id tag1)
                       (libgit-object-id (libgit-revparse-single repo "HEAD"))))
      (should (string= (libgit-tag-target-id tag2)
                       (libgit-object-id (libgit-revparse-single repo "HEAD^{tree}"))))
      (should (string= (libgit-object-id (libgit-tag-target tag1))
                       (libgit-object-id (libgit-revparse-single repo "HEAD"))))
      (should (string= (libgit-object-id (libgit-tag-target tag2))
                       (libgit-object-id (libgit-revparse-single repo "HEAD^{tree}")))))))

(ert-deftest tag-peel ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (run "git" "tag" "-a" "t1" "-m" "z" "HEAD")
    (run "git" "tag" "-a" "t2" "-m" "z" "t1")
    (run "git" "tag" "-a" "t3" "-m" "z" "t2")
    (let* ((repo (libgit-repository-open path))
           (tagid (read-file-nnl ".git/refs/tags/t3"))
           (tag (libgit-tag-lookup repo tagid)))
      (should (eq 'tag (libgit-tag-target-type tag)))
      (should (eq 'commit (libgit-typeof (libgit-tag-peel tag))))
      (should (string= (libgit-object-id (libgit-revparse-single repo "HEAD"))
                       (libgit-object-id (libgit-tag-peel tag)))))))

(ert-deftest tag-map ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (run "git" "tag" "-a" "tag1" "-m" "z" "HEAD")
    (run "git" "tag" "-a" "tag2" "-m" "z" "HEAD")
    (run "git" "tag" "-a" "tag3" "-m" "z" "HEAD")
    (run "git" "tag" "-a" "tag4" "-m" "z" "HEAD")
    (let* ((repo (libgit-repository-open path))
           (taglist (libgit-tag-list repo))
           (id1 (read-file-nnl ".git/refs/tags/tag1"))
           (id2 (read-file-nnl ".git/refs/tags/tag2"))
           (id3 (read-file-nnl ".git/refs/tags/tag3"))
           (id4 (read-file-nnl ".git/refs/tags/tag4"))
           data)
      (libgit-tag-foreach repo (lambda (&rest args) (push args data)))
      (setq data (reverse data))
      (should (equal taglist '("tag1" "tag2" "tag3" "tag4")))
      (should (equal data `(("refs/tags/tag1" ,id1)
                            ("refs/tags/tag2" ,id2)
                            ("refs/tags/tag3" ,id3)
                            ("refs/tags/tag4" ,id4)))))))
