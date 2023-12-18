(require 'asdf)

(defparameter data (uiop:read-file-lines "data.txt"))

(defun expand-rows (lines)
    (defun expand-rows-inner (lines acc)
        (let ((line (first lines))) (if line
            (expand-rows-inner (rest lines) (if (position #\# line)
                (cons line acc)
                (cons line (cons line acc))))
            (reverse acc))))
    (expand-rows-inner lines nil))

(defun rotate (lines)
    (loop for x from 0 to (- (length (first lines)) 1) collect
        (coerce (loop for y from 0 to (- (length lines) 1) collect
            (char (nth y lines) x)) 'string)))

(defun expand (lines)
    (rotate (expand-rows (rotate (expand-rows lines)))))

(defun find-stars (lines) (let ((stars nil))
    (loop for y from 0 to (- (length lines) 1) do
        (loop for x from 0 to (- (length (first lines)) 1) do
            (if (char= (char (nth y lines) x) #\#) (push (list x y) stars))))
    stars))

(defun taxicab-distance (a b)
    (+ (abs (- (first a) (first b))) (abs (- (second a) (second b)))))

(defun part-one () (let ((stars (find-stars(expand data))) (sum 0))
    (loop for a in stars do
        (loop for b in stars do
            (unless (equal a b) (setq sum (+ sum (taxicab-distance a b))))))
    (/ sum 2)))

(print (part-one))
