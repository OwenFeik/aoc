(require 'asdf)

(defparameter pipe-map (uiop:read-file-lines "example.txt"))

(defun off-grid (point) (let ((x (first point)) (y (second point))) (or
    (< x 0)
    (< y 0)
    (>= y (length pipe-map))
    (>= x (length (first pipe-map))))))

(defun adj-tiles (point) (let ((x (first point)) (y (second point)))
    (remove-if 'off-grid (list
        (list (- x 1) (- y 1))
        (list x (- y 1))
        (list (+ x 1) (- y 1))
        (list (- x 1) y)
        (list (+ x 1) y)
        (list (- x 1) (+ y 1))
        (list x (+ y 1))
        (list (+ x 1) (+ y 1))))))

(defun connected-tiles (point) (let* (
    (x (first point))
    (y (second point))
    (pipe (char (nth y pipe-map) x)))
    (remove-if 'off-grid (cond
        ((char= pipe #\|) (list (list x (+ y 1)) (list x (- y 1))))
        ((char= pipe #\-) (list (list (+ x 1) y) (list (- x 1) y)))
        ((char= pipe #\L) (list (list x (- y 1)) (list (+ x 1) y)))
        ((char= pipe #\J) (list (list x (- y 1)) (list (- x 1) y)))
        ((char= pipe #\7) (list (list x (+ y 1)) (list (- x 1) y)))
        ((char= pipe #\F) (list (list x (+ y 1)) (list (+ x 1) y)))
        ((char= pipe #\.) nil)
        ((char= pipe #\S) (remove-if-not
            (lambda (adj) (member point (connected-tiles adj) :test 'equal))
            (adj-tiles point)))))))

; breadth first search from start tile, returning maximum distance reached
(defun part-one () ())

(print (connected-tiles '(0 2)))
