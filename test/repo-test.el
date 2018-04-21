(ert-deftest repo-p ()
  (should (not (libgit-repository-p nil)))
  (should (not (libgit-repository-p t)))
  (should (not (libgit-repository-p 'symbol)))
  (should (not (libgit-repository-p "string")))
  (should (not (libgit-repository-p '(a b c))))
  (should (not (libgit-repository-p 1))))

(ert-deftest init-repo ()
  (with-temp-dir path
    (let ((repo (libgit-repository-init path)))
      (should (libgit-repository-p repo))
      (should (equal path (libgit-repository-workdir repo)))
      (should (equal (concat path ".git/") (libgit-repository-path repo))))))

(ert-deftest open-repo ()
  (with-temp-dir path
    (shell-command-to-string "git init")
    (let ((repo (libgit-repository-open path)))
      (should (libgit-repository-p repo))
      (should (equal path (libgit-repository-workdir repo)))
      (should (equal (concat path ".git/") (libgit-repository-path repo)))
      (should (not (libgit-repository-bare-p repo)))
      (should (libgit-repository-empty-p repo)))
    (should-error (libgit-repository-open-bare path) :type 'giterr)))

(ert-deftest open-repo-bare ()
  (with-temp-dir path
    (shell-command-to-string "git init --bare")
    (let ((repo (libgit-repository-open path)))
      (should (libgit-repository-bare-p repo))
      (should (libgit-repository-empty-p repo)))
    (let ((repo (libgit-repository-open-bare path)))
      (should (libgit-repository-bare-p repo))
      (should (libgit-repository-empty-p repo)))))

(ert-deftest open-repo-nonexistent ()
  (with-temp-dir path
    (should-error (libgit-repository-open path) :type 'giterr)))
