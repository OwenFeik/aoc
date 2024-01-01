(require 'asdf)

(defparameter data (uiop:read-file-lines "data.txt"))

(defun range (l) (- (length l) 1))

(defun roll-north (into from)
    (defun is-o (s i) (char= (char s i) #\O))
    (loop for i from 0 to (range into) do
        (if (and (char= (char from i) #\O) (char= (char into i) #\.)) (progn
            (setf (char into i) #\O)
            (setf (char from i) #\.))))
    (values into from))

(defun count-os (line)
    (loop for c across line sum (if (char= c #\O) 1 0)))

(defun total-load (m)
    (defun stress (i) (- (length m) i))
    (loop for i from 0 to (range m) sum
        (* (stress i) (count-os (nth i m)))))

(defun part-one ()
    (let ((m data) (n (length data))) (loop for i from 0 to (- n 2) do
        (loop for j from 0 to i do
            (roll-north (nth (- i j) m) (nth (+ (- i j) 1) m))))
            
        (total-load m)))

(print (part-one))
