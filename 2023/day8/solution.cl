(require 'asdf)


(defvar left)
(defvar right)
(defvar directions)
(defvar n)

(defun insert-entry (entry)
    (let* (
        (parts (uiop:split-string entry))
        (key (first parts))
        (left-val (subseq (third parts) 1 4))
        (right-val (subseq (fourth parts) 0 3)))
        (setf (gethash key left) left-val)
        (setf (gethash key right) right-val)))

(let ((data (uiop:read-file-lines "data.txt")))
    (setq directions (first data))
    (setq n (length directions))
    (setq left (make-hash-table :test 'equal))
    (setq right (make-hash-table :test 'equal))
    (loop for entry in (subseq data 2) do (insert-entry entry)))

(defun take-step (i pos)
    (if (char= (char directions i) #\L) (gethash pos left) (gethash pos right)))

(defun traverse-to (steps pos one-z)
    (if (or (and one-z (char= (char pos 2) #\Z)) (string= pos "ZZZ"))
        (values steps pos)
        (traverse-to (+ steps 1) (take-step (mod steps n) pos) one-z)))

(defun part-one () (traverse-to 0 "AAA" nil))

(defun ghosts ()
    (loop for key being the hash-keys of left
        when (char= (char key 2) #\A) collect (list 0 key)))

(defun period (ghost) (traverse-to (first ghost) (second ghost) t))

(defun part-two () (apply 'lcm (mapcar 'period (ghosts)))) 

(print (part-two))
