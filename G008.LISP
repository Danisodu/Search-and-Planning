;;;; Author(s): Group 8

;;;;IMPORTS

;;;; CONSTANTS
(defconstant max-shift 480)
(defconstant max-time-til-lunch 240)
(defconstant time-driving 40)

;;;; FUNCS
(defun task-time(task)
  (- (nth 3 task) (nth 2 task)))

(defun get-shift-time(to-do-tasks)
  (loop for task in to-do-tasks summing (task-time task)))

(defun is-intersected-task(to-compare-task new-task)
  (let((start2 (nth 2 new-task)) (end1 (nth 3 to-compare-task)))
    (return-from is-intersected-task (or (< start2 end1) (and (eq start2 end1) (/= (nth 0 new-task) (nth 1 to-compare-task)))))))

(defun check-new-task(new-task to-do-tasks))
 ;check if they intersect
 ;check if new task time exceeds max-shift (keep eating constraint in mind)
 ;

(defun sucessors (task))

(defun heuristic (task))

(defun faz-afetacao (tasks))
