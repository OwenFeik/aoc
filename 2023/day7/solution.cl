(require 'asdf)

(defparameter data
    (mapcar 'uiop:split-string (uiop:read-file-lines "data.txt")))

(defparameter FIVE_OF    7)
(defparameter FOUR_OF    6)
(defparameter FULL_HOUSE 5)
(defparameter THREE_OF   4)
(defparameter TWO_PAIR   3)
(defparameter ONE_PAIR   2)
(defparameter HIGH_CARD  1)

(defun add-char (c char-count-list)
    (let ((found nil))
        (loop for entry in char-count-list do
            (when (char= (first entry) c)
                (setf (second entry) (+ (second entry) 1))
                (setq found t)))
        (if found char-count-list (cons (list c 1) char-count-list))))

(defun count-cards (hand)
    (defun count-cards-inner (hand acc)
        (if (> (length hand) 0) (count-cards-inner
            (subseq hand 1)
            (add-char (uiop:first-char hand) acc)) acc))
    (mapcar 'second (count-cards-inner hand nil)))

(defun identify-hand (hand)
    (let ((counts (count-cards hand))) (cond
        ((and (first counts) (= (first counts) 5)) FIVE_OF)
        ((or (equal (first counts) 4) (equal (second counts) 4)) FOUR_OF)
        ((or (equal '(2 3) counts) (equal '(3 2) counts)) FULL_HOUSE)
        ((find 3 counts) THREE_OF)
        ((equal '(2 2) (remove-if (lambda (c) (/= c 2)) counts)) TWO_PAIR)
        ((find 2 counts) ONE_PAIR)
        (t HIGH_CARD))))

(defun improve-hand (score)
    (cond
        ((= score HIGH_CARD) ONE_PAIR)
        ((= score ONE_PAIR) THREE_OF)
        ((= score TWO_PAIR) FULL_HOUSE)
        ((= score THREE_OF) FOUR_OF)
        ((= score FULL_HOUSE) FOUR_OF)
        ((= score FOUR_OF) FIVE_OF)
        ((= score FIVE_OF) FIVE_OF)))

(defun handle-jokers (hand score)
    (let ((c (uiop:first-char hand))) (cond
        ((not c) score)
        ((char= c #\J) (handle-jokers (subseq hand 1) (improve-hand score)))
        (t (handle-jokers (subseq hand 1) score)))))

(defun identify-hand-jokers (hand)
    (handle-jokers hand (identify-hand (uiop:frob-substrings hand '("J")))))

(defun card-value-common (card))

(defun card-value (card) (cond
    ((char= card #\A) 14)
    ((char= card #\K) 13)
    ((char= card #\Q) 12)
    ((char= card #\J) 11)
    ((char= card #\T) 10)
    (t (digit-char-p card))))

(defun card-value-jokers (card)
    (if (char= card #\J) 1 (card-value card)))

(defun cmp-cards (a b) (< (card-value a) (card-value b)))

(defun cmp-hand-by-card (a b)
    (let (
        (cv-a (card-value (uiop:first-char a)))
        (cv-b (card-value (uiop:first-char b))))
        (if (= cv-a cv-b)
            (cmp-hand-by-card (subseq a 1) (subseq b 1))
            (< cv-a cv-b))))

(defun cmp-hands (a b)
    (if (string= a b) nil (let (
        (type-a (identify-hand a))
        (type-b (identify-hand b)))
        (if (= type-a type-b)
            (cmp-hand-by-card a b)
            (< type-a type-b)))))

(defun cmp-hand-bet-pairs (a b)
    (cmp-hands (first a) (first b)))

(defun part-one ()
    (let ((pairs (sort data 'cmp-hand-bet-pairs)))
        (apply '+ (loop for rank from 1 to (length pairs) collect
            (* rank (parse-integer (second (nth (- rank 1) pairs))))))))

(defun cmp-hand-by-card-jokers (a b)
    (let (
        (cv-a (card-value-jokers (uiop:first-char a)))
        (cv-b (card-value-jokers (uiop:first-char b))))
        (if (= cv-a cv-b)
            (cmp-hand-by-card-jokers (subseq a 1) (subseq b 1))
            (< cv-a cv-b))))

(defun cmp-hands-jokers (a b)
    (if (string= a b) nil (let (
        (type-a (identify-hand-jokers a))
        (type-b (identify-hand-jokers b)))
        (if (= type-a type-b)
            (cmp-hand-by-card-jokers a b)
            (< type-a type-b)))))

(defun cmp-hand-bet-pairs-jokers (a b)
    (cmp-hands-jokers (first a) (first b)))

(defun part-two ()
    (let ((pairs (sort data 'cmp-hand-bet-pairs-jokers)))
        (apply '+ (loop for rank from 1 to (length pairs) collect
            (* rank (parse-integer (second (nth (- rank 1) pairs))))))))

(print (part-two))
