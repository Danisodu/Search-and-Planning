;(load "g008.LISP")

;-------------------------------------------
;----------Empty Spaces Heuristic-----------
;-------------------------------------------

;;;empty-space-two-tasks: task x task -> integer
;; Calculates the IDLE time between two tasks            
(defun empty-space-two-tasks(t1 t2)
    (- (nth 2 t2) (nth 3 t1) )
)
 ;;;calc-empty-spaces: shift -> inteiro
;; Receives a shift (sorted) and returns the sum of empty spaces between tasks
(defun calc-sum-empty-spaces(shift)
    (let ((sum-empty-spaces 0) 
        (tasks (shift-tasks shift))
        (current)
        (next)
        (space)) 
        
        ;When using the heuristic this will never happen
        (if (eq tasks nil) (return-from calc-sum-empty-spaces 0))
        
        ;If only one element there can not be empty spaces
        (if (eq (list-length tasks) 1) (return-from calc-sum-empty-spaces 0))
        
        ;Calculate consecutive tasks empty space
        (loop for i from 0 to (- (list-length tasks) 1) do 
            (setf current (nth i tasks))
            (setf next (nth (+ i 1) tasks))
            (if next (progn
                            (setf space (empty-space-two-tasks current next))
                            (incf sum-empty-spaces space))))
                            
        sum-empty-spaces))


;;;empty-spaces-heuristic: state -> integer
;; Calculates the sum of all empty time spaces between consecutive tasks in all state shifts
(defun empty-spaces-heuristic (state)
    (let(
        (shifts (state-shifts state))
        (sum-all-shifts 0))
        
        (dolist (shift shifts)
            (incf sum-all-shifts (calc-sum-empty-spaces shift)))
    )
)
        
;-------------------------------------------
;------------Shifts Number Heuristic--------
;-------------------------------------------
(defun shifts-number-heuristic (state)
    (let(
        (shifts (state-shifts state)))
        
        (list-length shifts)))
        
        
        
;-------------------------------------------
;------------Spacial Consecutive Tasks------
;-------------------------------------------
;;;spacial-consecutive-tasks? task x task -> boolean
;; Check if two tasks are spacial consecutive, ie, if t1 ends where t2 starts      
(defun spacial-consecutive-tasks?(t1 t2)
    (equal (nth 1 t1) (nth 0 t2) )
)

(defun starts-in-L1? (shift)
    (let* ( (tasks (shift-tasks shift))
            (first-task (first tasks)))
            
        (equal (nth 0 first-task) 'L1)))

        
(defun ends-in-L1? (shift)
    (let* ( (tasks (shift-tasks shift))
            (last-task (nth (- (list-length tasks) 1) tasks)))
    (equal (nth 1 last-task) 'L1)))
    
    

;;; Shift -> integer
;; Calculates the number of all no-service trips needed to guarantee spacial continuity in one shift
(defun calc-no-service-trips(shift)
    (let ((sum-no-services-trips 0) 
        (tasks (shift-tasks shift))
        (current)
        (next)
        ) 
        
        ;Empty list has no trips at all, not really going to happen tho...
        (if (eq tasks nil) (return-from calc-no-service-trips 0))
        
        ;Check if trip to start in L1 is needed
        (if (not (starts-in-L1? shift)) (incf sum-no-services-trips 1))
        
        ;Check if trip to end in L1 is needed
        (if (not (ends-in-L1? shift)) (incf sum-no-services-trips 1))

        ;Calculate needed trips beetwen tasks
        (loop for i from 0 to (- (list-length tasks) 1) do 
            (setf current (nth i tasks))
            (setf next (nth (+ i 1) tasks))
            (if next 
                (if (not (spacial-consecutive-tasks? current next)) (incf sum-no-services-trips 1))))
                            
        sum-no-services-trips))


;;;empty-spaces-heuristic: state -> integer
;; Calculates the sum of all empty time spaces between consecutive tasks in all state shifts
(defun consecutive-tasks-heuristic (state)
    (let(
        (shifts (state-shifts state))
        (sum-all-shifts 0))
        
        (dolist (shift shifts)
            (incf sum-all-shifts (calc-no-service-trips shift)))
    )
)

