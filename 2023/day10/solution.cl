(require 'asdf)

(defparameter pipe-map (uiop:read-file-lines "data.txt"))

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

(defun start-tile ()
    (let ((point nil))
        (loop for y from 0 to (- (length pipe-map) 1) do
            (loop for x from 0 to (- (length (first pipe-map)) 1) do
                (if (char= (char (nth y pipe-map) x) #\S)
                    (setq point (list x y)))))
        point))

(defun part-one () (let (
    (visited nil)
    (next-frontier (list (cons 0 (start-tile))))
    (max-dist 0))
    (loop while next-frontier do (let ((frontier next-frontier))
        (setq next-frontier nil)
        (loop for tile in frontier do
            (let ((dist (first tile)) (point (rest tile)))
                (push point visited)
                (setq next-frontier (append (mapcar
                    (lambda (point) (cons (+ dist 1) point))
                    (remove-if
                        (lambda (point) (member point visited :test 'equal))
                        (connected-tiles point))) next-frontier))
                (setq max-dist (max max-dist dist))))))
    max-dist))

(print (part-one))
