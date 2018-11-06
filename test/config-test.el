(ert-deftest repository-config ()
  (with-temp-dir path
    (init)
    (should (libgit-config-p (libgit-repository-config (libgit-repository-open path))))))

(ert-deftest config-get ()
  (with-temp-dir path
    (init)
    (write ".git/config" "\
[user]
  email = someone@somewhere.com
  name = John Doe
")
    (let* ((repo (libgit-repository-open path))
           (config (libgit-repository-config repo))
           (snap (libgit-config-snapshot config)))
      (should-error (libgit-config-get-string config "something") :type 'giterr)
      (should (string= "someone@somewhere.com" (libgit-config-get-string snap "user.email")))
      (should (string= "John Doe" (libgit-config-get-string snap "user.name"))))))
