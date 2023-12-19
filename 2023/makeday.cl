(require 'asdf)

(defun write-file (dir file content)
    (with-open-file (f (concatenate 'string dir file)
        :direction :output
        :if-does-not-exist :create)
        (write-string content f)))

(loop for i from 1 to 25 do (let ((dir (format nil "day~d/" i)))
    (unless (uiop:directory-exists-p dir) (progn
        (ensure-directories-exist dir)
        (write-file dir "solution.cl" "(require 'asdf)")
        (write-file dir "data.txt" "")
        (write-file dir "example.txt" "")
        (write-line (concatenate 'string "Created " dir))
        (return)))))
