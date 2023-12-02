(require "asdf")

(defparameter data (uiop:read-file-lines "data.txt"))

(defparameter red 1)
(defparameter green 2)
(defparameter blue 3)

(defparameter max-red 12)
(defparameter max-green 13)
(defparameter max-blue 14)

(defun parse-pair (pair)
    (let* (
            (parts (uiop:split-string pair))
            (n (parse-integer (first parts)))
            (c (second parts))
            (colour (cond
                ((string-equal c "red") red)
                ((string-equal c "green") green)
                ((string-equal c "blue") blue))))
        (list n colour)))

(defun parse-round (round)
    (mapcar
        (lambda (pair) (parse-pair (string-trim " " pair)))
        (uiop:split-string round :separator ",")))

(defun parse-id (game)
    (parse-integer (second (uiop:split-string
        (first (uiop:split-string game :separator ":"))))))

(defun parse-game (game)
    (mapcar
        (lambda (round) (parse-round (string-trim " " round)))
        (uiop:split-string
            (second (uiop:split-string game :separator ":"))
            :separator ";")))

(defun invalid-pair (pair)
    (let (
        (n (first pair))
        (colour (second pair)))
        (cond 
            ((= colour red) (> n max-red))
            ((= colour green) (> n max-green))
            ((= colour blue) (> n max-blue)))))

(defun invalid-round (round)
    (find-if 'invalid-pair round))

(defun invalid-game (game)
    (find-if 'invalid-round game))

(defun valid-games ()
    (remove-if 'invalid-game (mapcar 'parse-game data)))

(defun max-pair-count (colour pairs)
    (print pairs)
    (reduce
        'max
        (mapcar 'first (remove-if-not
            (lambda (pair) (= (second pair) colour))
            pairs))
        :initial-value 0))

;  | round     |
; (((2 1) (1 2)))
; | game        |

(defun max-game-count (colour game)
    (print game)
    (apply 'max (mapcar (lambda (round) (max-pair-count colour round)) game)))

(defun part-one ()
    (defun game-score (line)
        (let
            ((rounds (parse-game line))) 
            (if (find-if 'invalid-round rounds) 0 (parse-id line))))
    (print (apply '+ (mapcar 'game-score data))))

(defun part-two ()
    (defun game-power (game) (*
        (max-game-count red game)
        (max-game-count green game)
        (max-game-count blue game)))
    (print (apply '+ (mapcar 'game-power (mapcar 'parse-game data)))))

(part-two)
