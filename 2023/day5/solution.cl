(require 'asdf)

(defun parse-table-line (line)
    (mapcar 'parse-integer
        (remove-if (lambda (s) (string= s "")) (uiop:split-string line))))

(defun parse-table (table)
    (mapcar 'parse-table-line
        (rest (uiop:split-string table :separator (string #\linefeed)))))

(defun parse-data ()
    (let ((entries (uiop:split-string (uiop:frob-substrings
        (uiop:read-file-string "data.txt")
        (list (format nil "~C~C" #\linefeed #\linefeed))
        "|") :separator "|")))
        (values
            (parse-table-line
                (second (uiop:split-string (first entries) :separator ":")))
            (mapcar 'parse-table (rest entries)))))

(defun lookup-range (table-entry key)
    (let* (
        (dst-start (first table-entry))
        (src-start (second table-entry))
        (range-len (third table-entry))
        (offset (- key src-start))
        (in-range (and (>= offset 0) (< offset range-len))))
    (if in-range (+ dst-start offset) nil)))

(defun lookup (table key)
    (or (first (remove-if-not 'identity (mapcar
        (lambda (table-entry) (lookup-range table-entry key)) table))) key))

(defun apply-tables (tables key)
    (if tables (apply-tables (rest tables) (lookup (first tables) key)) key))

(defun part-one () (multiple-value-bind (seeds tables) (parse-data)
    (print (apply 'min
        (mapcar (lambda (seed) (apply-tables tables seed)) seeds)))))

(defun into-pairs (vals)
    (if vals (cons (subseq vals 0 2) (into-pairs (subseq vals 2))) nil))

(defun part-two () (multiple-value-bind (seed-ranges tables) (parse-data)
    (let ((min-location 2147483647)) 
        (loop for pair in (into-pairs seed-ranges) do
            (let ((start (first pair)) (len (second pair)))
                (loop for seed from start to (- (+ start len) 1) do
                    (setq min-location
                        (min min-location (apply-tables tables seed))))))
        min-location)))

(print (part-two))