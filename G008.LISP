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
  time-used)

(defstruct state
  shifts
  to-assign-tasks)


;;;; FUNCS
(defun calc-shift-time (tasks)
  (let ((first-task (car tasks))
        (last-task (nth (1- (length tasks)) tasks)))
    (- (nth 3 last-task)
       (nth 2 first-task))))

(defun create-shift (tasks)
   (make-shift :tasks tasks
               :time-used (calc-shift-time tasks)))

(defun insert-task-in-position (position task tasks)
  (let((first-sub-seq (subseq tasks
                              0
                              position)))
    (if (eq first-sub-seq nil)
        (setf first-sub-seq '()))
  (nconc (nconc first-sub-seq
                (list task))
         (nthcdr position tasks))))

(defun add-task-to-shift (position task shift)
  (let ((new-tasks-list (insert-task-in-position position
                                                 task
                                                 (shift-tasks shift))))
  (setf (shift-tasks shift) new-tasks-list
        (shift-time-used shift) (calc-shift-time new-tasks-list))))

(defun add-drive-duration? (origin destination end-time)
  (if (not (eq origin destination))
      (setf end-time (+ end-time drive-duration)))
  (return-from add-drive-duration? end-time))

;;;empty-space-two-tasks: task x task -> integer
;; Calculates the IDLE time between two tasks
(defun empty-space-two-tasks (t1 t2)
  (- (nth 2 t2)
     (add-drive-duration? (nth 1 t1)
                          (nth 0 t2)
                          (nth 3 t1)))
)

(defun calc-1st-empty-space(last-task)
  (let((1st-empty-space-start-time (- (nth 3 last-task)
                    max-shift))
       (1st-empty-space (list 'L1 'L1 nil)))
  (if (< 1st-empty-space-start-time 0)
      (setf 1st-empty-space-start-time 0))
    (nconc 1st-empty-space
           (list 1st-empty-space-start-time))))

(defun has-space-to-lunch (tasks)
  (let ((first-task (nth 0 tasks))
        (tasks (cons (calc-1st-empty-space (nth (1- (length tasks)) tasks))
                     tasks))
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

;;;list can't be empty
(defun calc-empty-spaces (tasks)
  (let ((empty-spaces '())
        (last-task (nth (1- (length tasks)) tasks))
        (first-task (nth 0 tasks))
        (current)
        (next))
    (setf empty-spaces (list (list (calc-1st-empty-space last-task)
                                    first-task)))
    (loop for i from 0 to (- (length tasks) 2) do
          (setf current (nth i tasks)
                next (nth (1+ i) tasks))
          (setf empty-spaces (nconc empty-spaces
                                    (list (list current
                                          next)))))
    (nconc empty-spaces
           (list (list last-task
                 (list 'L1 'L1 (+ (nth 2 first-task) max-shift) nil))))))

(defun can-lunch (shift)
  (let ((correct 'T))
    (if (> (shift-time-used shift)
           max-time-til-lunch)
        (setf correct (has-space-to-lunch (shift-tasks shift))))
    (return-from can-lunch correct)))

(defun is-intersected (new-task other-task1 other-task2)
  (let ((new-task-start-time (nth 2 new-task))
        (new-task-end-time (nth 3 new-task))
        (new-task-origin (nth 0 new-task))
        (new-task-destination (nth 1 new-task))
        (other1-task-end-time (nth 3 other-task1))
        (other1-task-destination (nth 1 other-task1))
        (other2-task-origin (nth 0 other-task2))
        (other2-task-start-time (nth 2 other-task2)))
    (setf other1-task-end-time (add-drive-duration? new-task-origin
                                                    other1-task-destination
                                                    other1-task-end-time)
          new-task-end-time (add-drive-duration? other2-task-origin
                                                 new-task-destination
                                                 new-task-end-time))
      (or (> other1-task-end-time new-task-start-time)
          (> new-task-end-time other2-task-start-time))))

(defun find-position (new-task tasks)
  (let ((empty-spaces (calc-empty-spaces tasks)))
    (loop for empty-space in empty-spaces and pos from 0 do
          (multiple-value-bind(intersected) (is-intersected new-task
                                                            (nth 0 empty-space)
                                                            (nth 1 empty-space))
            (if (not intersected)
                (return-from find-position pos))))
  (return-from find-position -1)))

(defun update-sucessors-lst(shifts state sucessors)
  (setf (state-shifts state) shifts)
  (nconc sucessors (list state)))

(defun sucessors (state)
  (let ((next-task (pop (state-to-assign-tasks state)))
        (position)
        (current-shift)
        (shifts (state-shifts state))
        (shfts)
        (stte)
        (last-shift)
        (sucessors '()))
	
	;added to check if all nodes are already expanded, if yes return nil
	(if (eq next-task nil) (return-from sucessors nil))	
	
    (loop for pos from 0 to (1- (length shifts)) do
          (setf current-shift (copy-structure (nth pos shifts))
                shfts (copy-list shifts)
                stte (copy-structure state))
          (setf position (find-position next-task
                                        (shift-tasks current-shift)))
          (if (>= position 0)
              (progn
                (add-task-to-shift position
                                   next-task
                                   current-shift)
                (if (can-lunch current-shift)
                    (progn
                      (setf (nth pos shfts) current-shift)
                      (setf sucessors (update-sucessors-lst shfts stte sucessors)))))))
    (setf last-shift (create-shift (list next-task)))
    (if (can-lunch last-shift)
        (update-sucessors-lst (nconc shifts
                                    (list last-shift))
                              state
                              sucessors))))

(defun heuristic (state))

(defun faz-afetacao (tasks)
  (let((initial-state (make-state :shifts '()
                                  :to-assign-tasks tasks)))))

; (problem (cria-problema (initial-state
;                         (list 'sucessors)
;                        :objetivo? (eq (state-to-assign-tasks initial-state) '())
;                        :heuristica 'heuristic))))
