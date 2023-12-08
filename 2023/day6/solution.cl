(require 'asdf)

(defun parse-nums (str)
    (mapcar 'parse-integer (remove-if
        (lambda (a) (string= a ""))
        (uiop:split-string str))))

(defparameter data (mapcar
    (lambda (line) (parse-nums (second
        (uiop:split-string line :separator ":"))))
    (uiop:read-file-lines "data.txt")))

(defun range (max) (loop for i from 1 to (- max 1) collect i))

(defun distance (held total) (* held (- total held)))

(defun distances (total) (mapcar
    (lambda (held) (distance held total))
    (range total)))

(defun num-ways (total record)
    (length (remove-if (lambda (dist) (<= dist record)) (distances total))))

(defun part-one () (apply '* (mapcar 'num-ways (first data) (second data))))

(defun quadratic-formula (a b c)
    (defun qf-inner (a b d) (/ (+ (- b) d) (* 2 a)))
    (let ((discriminant (sqrt (- (expt b 2) (* 4 a c)))))
        (values
            (qf-inner a b discriminant)
            (qf-inner a b (- discriminant)))))

(defun round-up-in-range (held total record)
    (if (> (distance (floor held) total) record) (floor held) (ceiling held)))

(defun round-down-in-range (held total record)
    (if (> (distance (ceiling held) total) record) (ceiling held) (floor held)))

(defun part-two-inputs () (mapcar
    (lambda (line) (parse-integer (uiop:frob-substrings
        (second (uiop:split-string line :separator ":")) '(" "))))
    (uiop:read-file-lines "data.txt")))

(defun approx-bounds (total record) 
    (multiple-value-bind (least most)
        (quadratic-formula (- 1) total (- record))
        (+ (-
            (round-down-in-range most total record)
            (round-up-in-range least total record)) 1)))

(defun part-two () (let* ((inputs (part-two-inputs)))
    (approx-bounds (first inputs) (second inputs))))

;; (print (part-one))
(print (part-two))
