(require 'asdf)

(defparameter data (uiop:read-file-lines "data.txt"))

(defun num-len (num)
    (defun inner (num acc) (let ((n (floor num 10)))
        (if (= 0 n) acc (inner n (+ acc 1)))))
    (inner num 1))

; Returns list of ((num end-idx) (num end-idx) ...)
(defun nums-idx (line idx acc-list acc) (let (
    (c (uiop:first-char line))
    (reset-acc (if (not (= 0 acc)) (cons (list acc idx) acc-list) acc-list))
    (j (+ idx 1)))
    (cond
        ((not c) reset-acc)
        ((digit-char-p c) (nums-idx (subseq line 1) j acc-list
            (+ (* acc 10) (parse-integer (string c)))))
        (t (nums-idx (subseq line 1) j reset-acc 0)))))

(defun nums (line) (nums-idx line 0 nil 0))

(defun left-idx (num idx) (- idx (+ 1 (num-len num))))

(defun left (line num idx) (if (>= (left-idx num idx) 0)
    (let ((start (left-idx num idx))) (subseq line start (+ start 1)))
    ""))

(defun right (line idx)
    (if (<= (+ idx 1) (length line)) (subseq line idx (+ idx 1)) ""))

(defun span (line num idx)
    (subseq line (max 0 (left-idx num idx)) (min (+ idx 1) (length line))))

(defun around (prev line next num idx) 
    (concatenate 'string
        (if prev (span prev num idx) "")
        (left line num idx)
        (right line idx)
        (if next (span next num idx) "")))

(defun valid-num (prev line next pair)
    (defun is-symbol (char) (cond
        ((digit-char-p char) nil)
        ((char= char #\.) nil)
        (t t)))
    (defun contains-symbol (str) (find-if 'is-symbol str))
    (contains-symbol (around prev line next (first pair) (second pair))))

(defun line-sum (prev line next) (apply '+ (mapcar 'first (remove-if-not
    (lambda (pair) (valid-num prev line next pair))
    (nums line)))))

(defun part-one (prev lines) (let
    ((line (first lines)) (next (second lines)))
    (+ (if next (part-one line (rest lines)) 0) (line-sum prev line next))))

(defun digit-at (line idx)
    (and (and (>= idx 0) (< idx (length line))) (digit-char-p (char line idx))))

(defun eat-num (line idx start) (if (digit-at line idx)
    (eat-num line (+ idx 1) start)
    (parse-integer (subseq line start idx))))

(defun num-from (line idx)
    (if (and line (digit-at line idx))
        (if (digit-at line (- idx 1))
            (num-from line (- idx 1))
            (eat-num line idx idx))
        nil))

(defun adj-nums (prev line next idx) (remove-duplicates (remove nil (list
    (num-from prev (- idx 1))
    (num-from prev idx)
    (num-from prev (+ idx 1))
    (num-from line (- idx 1))
    (num-from line (+ idx 1))
    (num-from next (- idx 1))
    (num-from next idx)
    (num-from next (+ idx 1))))))

(defun asterisk-idxs (line idx)
    (if (> (length line) 0)
        (let ((acc (asterisk-idxs (subseq line 1) (+ idx 1))))
            (if (char= (uiop:first-char line) #\*)
                (cons idx acc)
                acc))
        nil))

(defun line-cogs-value (prev line next)
    (reduce '+ (mapcar (lambda (cog) (* (first cog) (second cog)))
        (remove-if-not (lambda (nums) (= 2 (length nums)))
            (mapcar
                (lambda (idx) (adj-nums prev line next idx))
                (asterisk-idxs line 0))))))

(defun part-two (prev lines) (let
    ((line (first lines)) (next (second lines)))
    (+
        (if next (part-two line (rest lines)) 0)
        (line-cogs-value prev line next))))

;; (print (part-one nil data))
(print (part-two nil data))
