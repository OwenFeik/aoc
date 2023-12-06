(require 'asdf)

(defparameter data (uiop:read-file-lines "data.txt"))

(defun parse-nums (str)
    (mapcar 'parse-integer (remove-if
        (lambda (a) (string= a ""))
        (uiop:split-string str))))

(defun parse-card (line) (let* (
    (parts (uiop:split-string (subseq line 10) :separator "|"))
    (winners (parse-nums (first parts)))
    (numbers (parse-nums (second parts)))) (values winners numbers)))

(defun card-score (line)
    (multiple-value-bind (winners numbers) (parse-card line)
        (let ((match-count (length (intersection winners numbers))))
            (if (> match-count 0) (expt 2 (- match-count 1)) 0))))

(defun part-one () (apply '+ (mapcar 'card-score data)))

(print (part-one))