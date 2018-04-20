(push (concat default-directory "build") load-path)
(require 'libegit2)

(defmacro with-temp-dir (varname &rest body)
  (declare (indent 1))
  (let ((path (concat temporary-file-directory "libegit2-test/")))
    `(unwind-protect
         (progn
           (make-directory ,path)
           (let ((,varname ,path) (default-directory ,path))
             ,@body))
       (delete-directory ,path 'recursive))))
