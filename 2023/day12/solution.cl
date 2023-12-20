(require 'asdf)

(defun parse-line (line) (let* (
    (parts (uiop:split-string line))
    (spring-map (first parts))
    (spring-nums (mapcar 'parse-integer
        (uiop:split-string (second parts) :separator ","))))
    (list spring-map spring-nums)))

(defparameter data
    (mapcar 'parse-line (uiop:read-file-lines "data.txt")))

(defun max-spring-prefix (s) (let ((i 0)) (loop while (and
    (< i (length s))
    (or (char= (char s i) #\?) (char= (char s i) #\#))) do (incf i)) i))

(defun try-prefix (s n) (if (and
    n
    (>= (max-spring-prefix s) n)
    (or (>= n (length s)) (char/= (char s n) #\#))) (+ n 1)))

(defun substr (s i) (if (>= i (length s)) "" (subseq s i)))

(defun cons-all (v ls)
    (mapcar (lambda (l) (cons v l)) ls))

(defun gen-combs (i line nums) (if (< i (length line))
    (let* (
        (rest-line (subseq line i))
        (d (try-prefix rest-line (first nums)))
        (c (uiop:first-char rest-line)))
        (cond
            ((and (char= c #\#) (not d)) nil)
            ((not d) (gen-combs (+ i 1) line nums))
            ((char= c #\#) (cons-all i (gen-combs (+ i d) line (rest nums))))
            (t (concatenate 'list (gen-combs (+ i 1) line nums)
                (cons-all i (gen-combs (+ i d) line (rest nums)))))))
    (if nums nil '(nil))))

(defun count-combs (line nums)
    (length (remove-duplicates (remove-if
        (lambda (l) (/= (length l) (length nums)))
        (gen-combs 0 line nums)))))

(defun part-one () (apply '+ (mapcar
    (lambda (pair) (count-combs (first pair) (second pair))) data)))

(print (part-one))
