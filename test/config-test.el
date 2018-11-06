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
      ;; Can't read from a live config object
      (should-error (libgit-config-get-string config "something") :type 'giterr)
      (should (string= "someone@somewhere.com" (libgit-config-get-string snap "user.email")))
      (should (string= "John Doe" (libgit-config-get-string snap "user.name"))))))

(ert-deftest config-set ()
  (with-temp-dir path
    (init)
    (let* ((repo (libgit-repository-open path))
           (config (libgit-repository-config repo))
           trans)

      (libgit-config-set-string config "user.name" "Captain Spiff")
      (let ((snap (libgit-config-snapshot config)))
        (should (string= "Captain Spiff" (libgit-config-get-string snap "user.name")))
        ;; Can't write to a config snapshot
        (should-error (libgit-config-set-string snap "user.name" "Zorg") :type 'giterr))

      ;; If the config is locked, writes aren't visible immediately
      (setq trans (libgit-config-lock config))
      (libgit-config-set-string config "user.name" "Wolfgang Amadeus")
      (let ((snap (libgit-config-snapshot config)))
        (should (string= "Captain Spiff" (libgit-config-get-string snap "user.name"))))
      (libgit-transaction-commit trans)
      (let ((snap (libgit-config-snapshot config)))
        (should (string= "Wolfgang Amadeus" (libgit-config-get-string snap "user.name")))))))
