(load "procura.lisp")
(load "G008.LISP")

;;;re-defining calcula-novo-g from procura.lisp
(defun calcula-novo-g (estado no-pai custo-transicao)
  "Calcula o novo valor de g para estado, sendo no-pai o seu pai."
    (declare (ignore estado) (ignore custo-transicao))


    (setf novo-g (calculate-state-cost estado))
    novo-g
)




(setf toAssignTasks '((L2 L1 1 25) (L10 L1 4 33) (L1 L2 14 40) (L1 L11 14 55) (L4 L1 16 37) ))


(setf problem (cria-problema (create-state '() toAssignTasks)
                               (list #'sucessors)
                               :objectivo? #'is-goal
                               :estado= #'equality
                               :heuristica #'number-to-assign-tasks
                               :custo #'calculate-state-cost))
                               
(setf sol (procura problem "a*"))

(print "a* solution:") (print sol)
;(print (last (car sol)))
                               
 

                               