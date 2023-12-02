(require "asdf")

(defparameter data (uiop:read-file-lines "data.txt"))

(defun part-one ()
    (defun calibration-value (entry)
        (let ((digits (remove-if-not 'digit-char-p (coerce entry 'list))))
            (+
                (* (parse-integer (string (first digits))) 10)
                (parse-integer (string (car (last digits)))))))

    (print (apply '+ (mapcar 'calibration-value data))))

(defparameter table (list 
    '("1" . 1)
    '("one" . 1)
    '("2" . 2)
    '("two" . 2)
    '("3" . 3)
    '("three" . 3)
    '("4" . 4)
    '("four" . 4)
    '("5" . 5)
    '("five" . 5)
    '("6" . 6)
    '("six" . 6)
    '("7" . 7)
    '("seven" . 7)
    '("8" . 8)
    '("eight" . 8)
    '("9" . 9)
    '("nine" . 9)))

(defun part-two () 
    (defun first-digit (entry)
        (let ((match (find-if
            (lambda (poss) (uiop:string-prefix-p (car poss) entry))
            table)))
        (if match (cdr match) (first-digit (subseq entry 1)))))

    (defun last-digit (entry) 
        (let ((match (find-if
            (lambda (poss) (uiop:string-suffix-p entry (car poss)))
            table)))
        (if match
            (cdr match)
            (last-digit (subseq entry 0 (- (length entry) 1))))))

    (defun calibration-value (entry)
        (+ (* (first-digit entry) 10) (last-digit entry)))

    (print (apply '+ (mapcar 'calibration-value data))))
