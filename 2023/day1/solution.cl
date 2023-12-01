(require "asdf")

(defparameter data (mapcar
    (lambda (line) (uiop:split-string line))
    (uiop:read-file-lines "data.txt")))

(print data)
(print (mapcar (lambda (entry) (parse-integer (second entry))) data))
