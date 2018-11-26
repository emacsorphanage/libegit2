(require 'cl-lib)

(defvar file-prefix (if (eq 'windows-nt system-type) "file:///" "file://"))

(defmacro with-temp-dir (varnames &rest body)
  (declare (indent 1))
  (let* ((cwd (if (listp varnames) temporary-file-directory varnames))
         (varnames (if (listp varnames) varnames (list varnames)))
         (bindings (mapcar (lambda (sym)
                             (list sym (format "%slibegit2-test-%s/"
                                               temporary-file-directory sym)))
                           varnames)))
    `(let (,@bindings)
       (unwind-protect
           (progn
             ,@(mapcar (lambda (sym) `(make-directory ,sym 'parents)) varnames)
             (let ((default-directory ,cwd))
               ,@body))
         ,@(mapcar (lambda (sym) `(ignore-errors (delete-directory ,sym 'recursive))) varnames)))))

(defmacro in-dir (dir &rest body)
  (declare (indent 1))
  `(let ((default-directory ,dir)) ,@body))

(defun run (&rest args)
  (with-temp-buffer
    (unless (= 0 (apply 'call-process (car args) nil (cons (current-buffer) t) nil (cdr args)))
      (error "failed to run '%s', output:\n%s" (mapconcat 'identity args " ") (buffer-string)))
    (buffer-string)))

(defun run-nnl (&rest args)
  (replace-regexp-in-string "\n*\\'" "" (apply 'run args)))

(defun run-fail (&rest args)
  (with-temp-buffer
    (apply 'call-process (car args) nil (cons (current-buffer) t) nil (cdr args))))

(defun write (filename content)
  (let ((dir (file-name-directory filename)))
    (when dir
      (make-directory dir 'parents)))
  (with-temp-file filename
    (unless (multibyte-string-p content)
      (set-buffer-multibyte nil))
    (insert content)))

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-string)))

(defun read-file-nnl (filename)
  (replace-regexp-in-string "\n*\\'" "" (read-file filename)))

(defun init (&rest args)
  (apply 'run "git" "init" args)
  (set-user))

(defun add (&rest args)
  (apply 'run "git" "add" args))

(defun commit (&optional msg)
  (run "git" "commit" "--allow-empty-message" "-m" (or msg "nothing")))

(defun create-branch (&optional name)
  (run "git" "checkout" "-b" (or name "unnamed")))

(defun checkout (name)
  (run "git" "checkout" name))

(defun merge (branch-name &optional msg)
  (run "git" "merge" "--no-ff" branch-name "-m" (or msg "merge commit")))

(defun commit-change (filename content &optional msg)
  (write filename content)
  (run "git" "add" filename)
  (commit msg))

(defun rev-parse (&optional spec)
  (run-nnl "git" "rev-parse" (or spec "HEAD")))

(defun set-user ()
  (run "git" "config" "user.name" "A U Thor")
  (run "git" "config" "user.email" "author@example.com"))

(defun path= (a b)
  (string= (file-truename a) (file-truename b)))

(unless (fboundp 'caddr)
  (defun caddr (x) (car (cdr (cdr x)))))
