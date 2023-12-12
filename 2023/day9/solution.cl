(require 'asdf)

(defparameter data (mapcar
    (lambda (line) (mapcar 'parse-integer (uiop:split-string line)))
    (uiop:read-file-lines "data.txt")))

(defun diffs (nums)
    (defun diffs-inner (nums acc)
        (if (>= (length nums) 2) (diffs-inner
            (subseq nums 1)
            (cons (- (second nums) (first nums)) acc)) acc))
    (reverse (diffs-inner nums nil)))

(defun extrapolate (nums)
    (let ((last-vals (last nums)) (seq nums))
        (loop while (not (every (lambda (num) (= num 0)) seq)) do
            (setq seq (diffs seq))
            (setq last-vals (cons (car (last seq)) last-vals)))
        (apply '+ last-vals)))

(defun part-one () (apply '+ (mapcar 'extrapolate data)))

(defun backstrapolate (nums)
    (let ((first-vals (list (first nums))) (seq nums))
        (loop while (not (every (lambda (num) (= num 0)) seq)) do
            (setq seq (diffs seq))
            (setq first-vals (cons (first seq) first-vals)))
        (reduce (lambda (acc val) (- val acc)) first-vals)))

(defun part-two () (apply '+ (mapcar 'backstrapolate data)))

(print (part-two))
