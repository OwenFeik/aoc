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

(defun card-matches (line)
    (multiple-value-bind (winners numbers) (parse-card line)
        (length (intersection winners numbers))))

(defun card-score (line)
    (let ((match-count (card-matches line)))
        (if (> match-count 0) (expt 2 (- match-count 1)) 0)))

(defun part-one () (apply '+ (mapcar 'card-score data)))

(defun part-two (lines counts)
    (let* (
        (line (first lines))
        (matches (if line (card-matches line) 0))
        (n (if (first counts) (first counts) 0)))
        (loop for i from 1 to n do
            (loop for i from 1 to matches do
                (setf (elt counts i) (+ (elt counts i) 1))))
        (if lines
            (+ n (part-two (rest lines) (rest counts)))
            0)))

(print (part-one))
(print (part-two data
    (loop for i from 1 to (length data) collect 1)))
