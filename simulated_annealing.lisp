(load "sondagem_iterativa.lisp")
(setf *random-state* (make-random-state t))

(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;;; create-random-initial-solution: problema -> estado (solucao)
;;  usa uma sondagem iterativa para gerar uma solucao valida aleatoria
(defun create-random-initial-solution (problem)
    (sondagem-iterativa problem)
)


;;; neighbour: state -> state
;;  Receives a state and returns a state similar to the one received but with a small change
;;  The change in this case is one random task is moved to a random shift
(defun neighbour(state problema)
    (let ((shifts (state-shifts state))
        (shift-tasks)
        (random-shift-index)
        (random-shift)
        (random-task-index)
        (random-task)
        (new-incomplete-state)
        (sucessores)
        (gera-sucessores (car (problema-operadores problema)))
        )
        (print "initial state:")
        (print state)
        (setf random-shift-index (random (list-length shifts))) ;indice do shift
                ;(print "random-shift-index:")
                ;(print random-shift-index)
                ;(print "shifts:")
                ;(print shifts)
        (setf random-shift (nth random-shift-index shifts)) ;estrutura shift
                ;(print "random-shift:")
                ;(print random-shift)

        (setf shift-tasks (shift-tasks random-shift)) ;lista de shifts da estrutura shit escolhida
        (setf random-task-index (random (list-length shift-tasks))) ;
                        ;(print "random-task-index:")
                        ;(print random-task-index)

                        ;(print "shift-tasks:")
                        ;(print shift-tasks)
        (setf random-task (nth random-task-index shift-tasks))
        ;Change to-assign-tasks slot
        (setf new-incomplete-state (copy-structure state))

        (setf (state-to-assign-tasks new-incomplete-state) (push random-task (state-to-assign-tasks new-incomplete-state)))
        
        ;Remove random task from the random shift
                        ;(print "shift-tasks antes:")
                        ;(print shift-tasks)
        (setf shift-tasks (remove-nth random-task-index shift-tasks)) ;eliminada tarefa da lista de tasks do shift
                        ;(print "shift-tasks depois:")
                        ;(print shift-tasks)
         
         
         
         (cond ((equal shift-tasks nil)
                    (setf shifts (remove-nth random-shift-index shifts)) ;se o turno fica sem tasks e' eliminado
                ) 
                (t 
                    (setf (shift-tasks random-shift) shift-tasks) ;atualizar o random-shift com a nova lista de tasks
                    (setf (shift-time-used random-shift) (calc-shift-time shift-tasks)) ;atualizar o random-shift com novo time-used
                    (setf (nth random-shift-index shifts) random-shift) ;atualizar a lista de shifts com o alterado random_shift 
                )
         )
         
        
        ;Change shifts slot
        (setf (state-shifts new-incomplete-state) shifts)

        (print "new-incomplete-state:")
        (print new-incomplete-state)
        
        ;Generate new possible sucessors, from the created "new-incomplete-state"
        (setf sucessores (funcall gera-sucessores new-incomplete-state))

        ;Select a random sucessor
            (print "sucessores:")
            (print sucessores)
        (nth (random (list-length sucessores)) sucessores)
          
    )
)       

(defun acceptance-probability (old-cost new-cost temp)
(print old-cost)
(print new-cost)
(print temp)
    (let ((res))
    
        (cond ((< new-cost old-cost) 
                (setf res 1)
               )
               (t
                
                (setf res (exp  ( / (- new-cost old-cost) temp)))
               )
        )
        
        res
    )
)

(defun random-prob ()
    ( / (+ (random 100) 1) 100.0)
)
    
    
(defun simulated_annealing (problema)
(print "entrei")
    (let (
        (initial-solution (create-random-initial-solution problema))
        (temp 1.0)
        (temp-min 0.00001)
        (alpha  0.9)
        (sol-nova)
        (sol-nova-custo)
        (sol)
        (old-cost)
        (acc_prob)
        (cost-scale-down 1)
        )
        
        (setf sol initial-solution)
        (setf old-cost (* (calculate-state-cost sol) cost-scale-down))
        (print 'oi)
        
        (loop while (> temp temp-min) do
            (setf sol-nova (neighbour sol problema))
            (setf sol-nova-custo (* (calculate-state-cost sol) cost-scale-down))
            (setf acc_prob (acceptance-probability old-cost sol-nova-custo temp))
            (if (>= acc_prob (random-prob))
                    (progn 
                        (setf sol sol-nova)
                        (setf old-cost  sol-nova-custo)
                    )
            )
            
            (setf temp (* temp alpha))
        )
        
        sol
    )
)




 (setf toAssignTasks '( ('L2 'L1 1 25) ('L1 'L2 34 60) ('L5 'L1 408 447) )) 
 (setf problem (cria-problema (create-state '() toAssignTasks)
                               (list #'sucessors)
                               :objectivo? #'is-goal
                               :estado= #'equality))
 (setf sol-inicial (sondagem-iterativa problem))
 (neighbour sol-inicial problem)
 (print (simulated_annealing problem))