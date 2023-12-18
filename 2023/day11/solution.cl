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

(defun find-blank-rows (lines) (let ((blanks nil))
    (loop for i from 0 to (- (length lines) 1) do
        (unless (position #\# (nth i lines)) (push i blanks)))
    blanks))

(defparameter blank-rows (find-blank-rows data))
(defparameter blank-cols (find-blank-rows (rotate data)))
(defparameter blank-expansion 999999)

(defun adapt-coord (point)
    (let* ((x (first point)) (y (second point)) (cx x) (cy y))
        (loop for i in blank-cols do
            (if (< i x) (setq cx (+ cx blank-expansion))))
        (loop for i in blank-rows do
            (if (< i y) (setq cy (+ cy blank-expansion))))
        (list cx cy)))

(defun part-two ()
    (let ((stars (mapcar 'adapt-coord (find-stars data))) (sum 0))
        (loop for a in stars do
            (loop for b in stars do
                (setq sum (+ sum (taxicab-distance a b)))))
        (/ sum 2)))

(print (part-two))
