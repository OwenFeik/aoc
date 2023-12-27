(require 'asdf)

(defparameter maps (mapcar
    (lambda (m) (uiop:split-string m :separator (string #\newline)))
    (uiop:split-string (uiop:frob-substrings
        (uiop:read-file-string "data.txt")
        (list (coerce '(#\newline #\newline) 'string))
        "$") :separator "$")))

(defun rotate (lines)
    (loop for x from 0 to (- (length (first lines)) 1) collect
        (coerce (loop for y from 0 to (- (length lines) 1) collect
            (char (nth y lines) x)) 'string)))

(defun is-reflection (left right)
    (if (and left right) (and
        (string= (first left) (first right))
        (is-reflection (rest left) (rest right))) t))

(defun find-reflection (m &optional x)
    (defun reflect-index (m k) (loop for i from 1 to (- (length m) 1) do 
        (if (and
            (not (equal (* i k) x))
            (is-reflection (reverse (subseq m 0 i)) (subseq m i)))
                (return (* i k)))))
    (or (reflect-index m 100) (reflect-index (rotate m) 1)))

(defun part-one () (apply '+ (mapcar 'find-reflection maps)))

(defun flip-symb (s) (if (char= s #\#) #\. #\#))

(defun flip-cell (m i) (multiple-value-bind (y x) (floor i (length (first m)))
    (let ((s (nth y m))) (concatenate 'list
        (subseq m 0 y)
        (list (concatenate 'string
            (subseq s 0 x)
            (string (flip-symb (char s x)))
            (subseq s (min (+ x 1) (length s)))))
        (subseq m (min (+ y 1) (length m)))))))
    
(defun find-smudge-reflection (m)
    (let ((x (find-reflection m)))
        (loop for i from 0 to (- (* (length m) (length (first m))) 1) do
            (let ((val (find-reflection (flip-cell m i) x)))
                (if val (return val))))))

(defun part-two () (apply '+ (mapcar 'find-smudge-reflection maps)))

(print (part-two))
