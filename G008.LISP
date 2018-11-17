;;;; Author(s): Group 8

;;;;IMPORTS


;;;; CONSTANTS
(defconstant max-shift 480)
(defconstant max-time-til-lunch 240)
(defconstant drive-duration 40)
(defconstant eat-duration 40)


;;;; STRUCTS
(defstruct shift
  tasks
  time-used
  empty-space
  meal-taken)

(defstruct state
  shifts
  to-assign-tasks)


;;;; FUNCS
(defun create-shift()
   (make-shift :tasks '()
               :time-used 0
               :empty-space 0
               :meal-taken nil))

(defun task-time(task)
  (- (nth 3 task) (nth 2 task)))

(defun add-task-to-shift(task shift)
  (setf (shift-tasks shift) (cons task (shift-tasks shift))
        (shift-time-used shift) (+ (task-time task) (shift-time-used shift)))
  (return-from add-task-to-shift shift))

;not sure if needed yet
(defun insert-task(position task tasks)
  (return-from insert-task (cons (cons (subseq tasks 0 position) task)
                                 (nthcdr position tasks))))
								 
								 
	

	
;;;empty-space-two-tasks: task x task -> integer
;; Calculates the IDLE time between two tasks			 
(defun empty-space-two-tasks(t1 t2)
	(- (nth 2 t2) (nth 3 t1) )
)

;;;calc-empty-spaces: shift -> inteiro
;; Receives a shift (sorted) and returns the max. interval between tasks (without driving)
;; Caso a lista seja vazia esta a devolver o tempo maximo de um turno (max-shift)
;; Caso a lista so tenha um elemento esta a devolver max-shift - tempo desse turno
;; Para os casos normais (len >= 2) devolve o tamanho do maior "buraco")
(defun calc-empty-spaces(shift)
	(let ((max-space 0) 
			(tasks (shift-tasks shift))
			(current)
			(next)
			(space)) 

		(print tasks)
		
		;Special case: If Empty Shift (check if actually needed)
		(if (eq tasks nil) (return-from calc-empty-spaces max-shift))
		
		;Special Case: If only one element (check if actually needed)
		(if (eq (list-length tasks) 1) (return-from calc-empty-spaces (- max-shift (task-time (nth 0 tasks)))))
		
		
		;Calculate consecutive tasks empty space
		(loop for i from 0 to (- (list-length tasks) 1) do 
			(setf current (nth i tasks))
			(setf next (nth (+ i 1) tasks))
			(cond (next 
						(setf space (empty-space-two-tasks current next))
						(print space)
						(if (> space max-space) (setf max-space space)))))
	max-space
	)
)





(defun add-drive-duration?(origin destination end-time)
  (if (/= origin destination)
      (setf end-time (+ end-time drive-duration)))
  (return-from add-drive-duration? end-time))

(defun is-intersected(new-task prev-task)
  (let((new-task-start-time (nth 2 new-task))
       (new-task-end-time (nth 3 new-task))
       (new-task-origin (nth 0 new-task))
       (new-task-destination (nth 1 new-task))                    
       (prev-task-start-time (nth 2 prev-task))
       (prev-task-end-time (nth 3 prev-task))
       (prev-task-origin (nth 0 prev-task))
       (prev-task-destination (nth 1 prev-task))
       (further-start nil)
       (closer-end nil))
    (if (< new-task-start-time prev-task-start-time)
        (setf closer-end (add-drive-duration? new-task-destination prev-task-origin new-task-end-time)
              further-start prev-task-start-time)
      (setf closer-end (add-drive-duration? prev-task-destination new-task-origin prev-task-end-time)
            further-start new-task-start-time))
    (return-from is-intersected (> closer-end further-start))))

(defun calc-intersection (new-task tasks)
  ;(let((possible-positions '())
  (loop for task in tasks do
        (if (is-intersected new-task task)
            (return-from calc-intersection 'T)))
  (return-from calc-intersection nil))

(defun check-new-task(new-task shift)
  (let((new-task-position nil)
       (new-task-duration (task-time new-task))
       (tasks (shift-tasks shift))
       (current-shift-duration (shift-time-used shift)))
    ;check if the task interesects with any of the existing tasks and if the new task doesn't exceed max shift time
    (if (and (not (calc-intersection new-task tasks))
             (< (+ current-shift-duration new-task-duration) max-shift))
        (if (not (shift-meal-taken shift))
            ;if the new task time + current shift time exceeds time to lunch, then lunch will be made and the task won't be added
            ;maybe it'd be best to see if there's an empty space where the worker can have lunch so the task can be added as well
            (if (>= (+ current-shift-duration new-task-duration) max-time-til-lunch)
                (setf (shift-meal-taken shift) 'T
                       (shift-time-used shift) (+ current-shift-duration eat-duration))
              (setf shift (add-task-to-shift new-task shift)))))
    (return-from check-new-task shift)))

(defun sucessors (state))

(defun heuristic (state))

(defun faz-afetacao (tasks)
  (let((initial-state (make-state :shifts '()
                                  :to-assign-tasks tasks)))))
  ; (problem (cria-problema (initial-state
   ;                         (list 'sucessors)
    ;                        :objetivo? (eq (state-to-assign-tasks initial-state) '())
    ;                        :heuristica 'heuristic))))
