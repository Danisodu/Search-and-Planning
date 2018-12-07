;;;; Author(s): Group 8


(in-package :user)

;;;;IMPORTS
;(load "C:/Users/X541U/OneDrive/IST/Search-and-Planning/procura.lisp")
(load "procura.lisp")

;-------------------------------------------------------------------------
;----------------------------------CONSTANTS------------------------------
;-------------------------------------------------------------------------

(defconstant max-shift 480)
(defconstant max-time-til-lunch 240)
(defconstant drive-duration 40)
(defconstant eat-duration 40)
(defconstant max-sucessors 3)

;-------------------------------------------------------------------------
;-----------------------------------STRUCTS-------------------------------
;-------------------------------------------------------------------------

(defstruct shift
  tasks
  time-used)

(defstruct state
  shifts
  to-assign-tasks
  heuristic-value)


;-------------------------------------------------------------------------
;---------------------------------HEURISTICS------------------------------
;-------------------------------------------------------------------------

(defun number-to-assign-tasks (state)
  (length (state-to-assign-tasks state)))


;-------------------------------------------------------------------------
;---------------------------------ALGORITHMS------------------------------
;-------------------------------------------------------------------------
(defun sort-by-time(tasks)
  (sort tasks #'< :key #'fourth)
  (sort tasks #'< :key #'third))

(defun sort-by-heuristic-value (states)
  (loop for state in states do
        (setf (state-heuristic-value state) (+ (calculate-state-cost state) 
                                              (number-to-assign-tasks state
))))

  (sort states #'< :key #'state-heuristic-value))

(defun ILDS-iteration (state discrepancies depth)
  (let ((best-discrepancy-res)
        (best-discrepancy-val most-positive-fixnum)
        (res)
        (sucss (sort-by-heuristic-value (sucessors state))))
    
    (if (is-goal state)
        (if (= discrepancies 0)
              (return-from ILDS-iteration state)
          (return-from ILDS-iteration nil)))

    (if (and (> discrepancies 0)
             (> (length sucss) 1))
        (progn
          (setf res (ILDS-iteration (nth 1 sucss) (1- discrepancies) (1- depth)))

          (if (not (null res))
              (if (< (state-heuristic-value res)
                     best-discrepancy-val)
                  (setf best-discrepancy-res res
                        best-discrepancy-val (state-heuristic-value res))))))

    (if (> depth discrepancies)
        (progn
          (setf res (ILDS-iteration (car sucss) discrepancies (1- depth)))

          (if (not (null res))
              (if (< (state-heuristic-value res)
                     best-discrepancy-val)
                  (setf best-discrepancy-res res
                        best-discrepancy-val (state-heuristic-value res))))))

    best-discrepancy-res))

(defun ILDS (problem)
  (let* ((initial-state (problema-estado-inicial problem))
        (res)
        (discrepancies 0)
        (depth)
        (best-solution initial-state))

    (setf depth (length (state-to-assign-tasks initial-state)))

    (loop for k from 0 to depth do
          (print k)
          (setf res (ILDS-iteration initial-state discrepancies depth))
          
          (print res)
          
          (if (and (not (null res))
                   (< (state-heuristic-value res) 
                      (state-heuristic-value best-solution)))
              (setf best-solution res))

          (incf discrepancies))

    best-solution))


;-------------------------------------------------------------------------
;---------------------------------FUNCS-----------------------------------
;-------------------------------------------------------------------------
(defun final-list (state)
  (let((shifts (state-shifts state)))

  (loop for shift in shifts collect (shift-tasks shift))))

(defun add-drive-duration? (origin destination end-time)
  (if (not (eq origin destination))
      (setf end-time (+ end-time drive-duration)))
  (return-from add-drive-duration? end-time))


;nao contempla a situacao de turnos com menos de 6h
(defun calc-shift-time (tasks)
  (let ((last-task (nth (1- (length tasks)) tasks))
        (start-time (nth 2 (car tasks))))
    (if (not (equalp (nth 0 (car tasks)) 'L1))
        (setf start-time (- start-time drive-duration)))
    (- (add-drive-duration? (nth 1 last-task) 'L1 (nth 3 last-task))
       start-time)))

(defun create-shift (tasks)
   (make-shift :tasks tasks
               :time-used (calc-shift-time tasks)))

(defun create-state (shifts toAssignTasks)
  (make-state :shifts shifts
              :to-assign-tasks toAssignTasks
              :heuristic-value (length toAssignTasks)))

(defun add-task-to-shift (task shift)
  (let ((new-tasks-list (nconc (shift-tasks shift) (list task))))

  (setf (shift-tasks shift) new-tasks-list
        (shift-time-used shift) (calc-shift-time new-tasks-list))))

(defun empty-space-two-tasks (t1 t2)
  (- (nth 2 t2)
     (add-drive-duration? (nth 1 t1)
                          (nth 0 t2)
                          (nth 3 t1))))

(defun has-space-to-lunch (tasks)
  (let ((first-task (nth 0 tasks))
        (pos 0)
        (current)
        (next)
        (shift-time 0))

    (loop while (and (<= pos
                        (- (length tasks) 2))
                     (<= shift-time max-time-til-lunch)) do
        (setf current (nth pos tasks)
              next (nth (1+ pos) tasks)
              pos (1+ pos))
        (setf shift-time (calc-shift-time (list first-task
                                                next)))
        (if (>= (empty-space-two-tasks current
                                       next)
                eat-duration)
            (return-from has-space-to-lunch 'T)))))

(defun can-lunch (shift)
  (let ((correct 'T))
    (if (> (shift-time-used shift)
           max-time-til-lunch)
        (setf correct (has-space-to-lunch (shift-tasks shift))))
    (return-from can-lunch correct)))

(defun is-intersected (task1 task2)
  (let ((task2-start-time (nth 2 task2))
        (task2-origin (nth 0 task2))
        (task1-end-time (nth 3 task1))
        (task1-destination (nth 1 task1)))

    (setf task1-end-time (add-drive-duration? task2-origin
                                              task1-destination
                                              task1-end-time))

      (> task1-end-time task2-start-time)))

(defun fits-in-shift (new-task tasks)
  (let ((last-task (nth (1- (length tasks)) tasks))
        (first-task (nth 0 tasks))
        (shift-end))

    (setf shift-end (list 'L1 'L1 (+ (nth 2 first-task) max-shift) nil))

    (if (and (not (is-intersected last-task new-task))
             (not (is-intersected new-task shift-end)))
        (return-from fits-in-shift 'T))))


 (defun copy-shift (shift-old)

    (let (
        (shift-new (copy-structure shift-old))
        (shift-tasks-copy)
        )
        (setf shift-tasks-copy (copy-list (shift-tasks shift-old)))

        (setf (shift-tasks shift-new) shift-tasks-copy)

        shift-new

    )
 )

 (defun copy-state (state-old)
    (let (
        (state-new (copy-structure state-old)) ;é estrutura
        (shifts-new (mapcar #'copy-structure (state-shifts state-old)))
        (to-assign-tasks-new (copy-list (state-to-assign-tasks state-old)))
        )

        ;copy each shift to new from the old
        (loop for i from 0 to (- (list-length shifts-new) 1) do
             (setf (nth i shifts-new) (copy-shift (nth i shifts-new)))
        )
        (setf (state-shifts state-new) shifts-new)

        ;copy to to-assign-tasks
        (setf (state-to-assign-tasks state-new) to-assign-tasks-new)

        state-new

    )
 )

(defun sucessors (state)
  (let* ((state2 (copy-state state))
         (next-task (pop (state-to-assign-tasks state2)))
         (current-shift)
         (shifts (state-shifts state))
         (stte)
         (pos 0)
         (new-shift)
         (sucessors '()))

   (if (null next-task) (return-from sucessors nil))

   (loop while (and (< pos (length shifts))
                    (< (length sucessors) max-sucessors)) do

         (setf current-shift (copy-shift (nth pos shifts))
               stte (copy-state state2))

         (if (fits-in-shift next-task
                            (shift-tasks current-shift))
             (progn
               (add-task-to-shift next-task current-shift)

               (if (can-lunch current-shift)
                   (progn
                     (setf (nth pos (state-shifts stte)) current-shift)
                     (setf sucessors (nconc sucessors (list stte)))))))

         (incf pos))

   (if (= (length sucessors) 0)
        (progn
          (setf new-shift (create-shift (list next-task)))

          (if (can-lunch new-shift)
              (progn
                (setf (state-shifts state2) (nconc (state-shifts state2) (list new-shift))) 
                (setf sucessors (list state2))))))
           
   sucessors))

;;;calculate-state-cost: state -> integer
;; Receives a state and return the cost of that state, based on the time of each shift
(defun calculate-state-cost (state)
    (let ((shifts (state-shifts state))
          (shift-time)
          (sum-all-shifts 0)
         )
        (dolist (shift shifts)
            (setf shift-time (shift-time-used shift))
            (if (< shift-time (* 6 60)) (setf shift-time (* 6 60))) ;if less than 6h its considered 6h used
            (incf sum-all-shifts shift-time)
         )

        sum-all-shifts
    ))

(defun is-goal (state)
  (eq (length (state-to-assign-tasks state)) 0))

(defun equality (s1 s2)
  (equalp s1 s2))

(defun faz-afetacao (toAssignTasks strategy)
 (let ((solution nil)
       (problem (cria-problema (create-state '() toAssignTasks)
                               (list #'sucessors)
                               :objectivo? #'is-goal
                               :estado= #'equality)))

       (cond ((equal strategy "melhor.abordagem")
              )
             ((equal strategy "a*.melhor.heuristica")
              )
             ((equal strategy "a*.melhor.heuristica.alternativa")
              ;(setf (problema-heuristica problem) "")
              (set solution "(car (procura problem strategy))")
              )
             ((equal strategy "sondagem.iterativa")
              )
             ((equal strategy "ILDS")
              (setf solution (ILDS problem))
              )
             ((equal strategy "abordagem.alternativa")
              )
       )

       (final-list solution)))