(ert-deftest repo-p ()
  (should (not (git-repository-p nil)))
  (should (not (git-repository-p t)))
  (should (not (git-repository-p 'symbol)))
  (should (not (git-repository-p "string")))
  (should (not (git-repository-p '(a b c))))
  (should (not (git-repository-p 1))))

(ert-deftest init-repo ()
  (with-temp-dir path
    (let ((repo (git-repository-init path)))
      (should (git-repository-p repo))
      (should (equal path (git-repository-workdir repo)))
      (should (equal (concat path ".git/") (git-repository-path repo))))))

(ert-deftest open-repo ()
  (with-temp-dir path
    (shell-command-to-string "git init")
    (let ((repo (git-repository-open path)))
      (should (git-repository-p repo))
      (should (equal path (git-repository-workdir repo)))
      (should (equal (concat path ".git/") (git-repository-path repo)))
      (should (not (git-repository-bare-p repo)))
      (should (git-repository-empty-p repo)))
    (should-error (git-repository-open-bare path) :type 'giterr)))

(ert-deftest open-repo-bare ()
  (with-temp-dir path
    (shell-command-to-string "git init --bare")
    (let ((repo (git-repository-open path)))
      (should (git-repository-bare-p repo))
      (should (git-repository-empty-p repo)))
    (let ((repo (git-repository-open-bare path)))
      (should (git-repository-bare-p repo))
      (should (git-repository-empty-p repo)))))

(ert-deftest open-repo-nonexistent ()
  (with-temp-dir path
    (should-error (git-repository-open path) :type 'giterr)))
