;;;; Author: Daniela Duarte 78542

;;;; CONSTANTS
(defconstant tab-length 4)
(defconstant last-position 15)
(defconstant first-position 0)

;;;; FUNCS
(defun get-index-by-value (n list)
  (loop for index from 0 below (list-length list) do
        (when (equal (nth index list) n) (return-from get-index-by-value index)))
  (return-from get-index-by-value NIL))

(defun array-to-list (array)
   (map 'list
           #'identity
           (make-array (array-total-size array)
                             :element-type (array-element-type array)
                             :displaced-to array)))

(defun list-to-array (list)
  (let ((array (make-array '(4 4))))
     (loop for n in list and position from first-position do
           (progn
             (multiple-value-bind (i j) (floor position tab-length)
             (setf (aref array i j) n))))
   (return-from list-to-array array)))

(defun correct-position (n)
 (if (equal n nil)
      (floor last-position tab-length)
   (floor (1- n) tab-length)))

(defun move-piece (state i k)
   (let ((i_val (nth i state))
         (k_val (nth k state))
         (state-copy (copy-tree state)))
     (setf (elt state-copy i) k_val (elt state-copy k) i_val)
     (return-from move-piece state-copy)))

(defun sucessors (state)
  (let ((neighborsPos nil) (neighbors nil) (emptyTileIdx (get-index-by-value nil state)))
    (setf neighborsPos (list (1- emptyTileIdx) (1+ emptyTileIdx) (- emptyTileIdx tab-length) (+ emptyTileIdx tab-length)))
    (loop for pos in neighborsPos do
          (if (and (<= pos last-position) (>= pos first-position))
            (push (move-piece state emptyTileIdx pos) neighbors)))
    (return-from sucessors neighbors)))

(defun equality (s1 s2)
  (equal s1 s2))

(defun manhattan-distance (n npos)
  (multiple-value-bind (correctX correctY) (correct-position n)
    (multiple-value-bind (currentX currentY) (correct-position npos)
      (+ (abs (- correctX currentX)) (abs (- correctY currentY))))))

(defun heuristic (state)
  (loop for value in state and position from 1
        sum (manhattan-distance value position)))

(defun solve-problem (initialState strategy)
  (let ((solution nil)
        (problem (cria-problema (array-to-list initialState)
                                (list 'sucessors)
                                :estado-final '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 nil)
                                :heuristica 'heuristic
                                :estado= 'equality)))
    (setf solution (car (procura problem strategy)))
    (loop for state in solution collect
         (list-to-array state))))
