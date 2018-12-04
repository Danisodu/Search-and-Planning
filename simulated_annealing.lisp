(load "procura.lisp")
(load "G008.LISP")
(load "sondagem_iterativa.lisp")
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
    (1-samp problem)
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
        )
        
        (setf random-shift-index (random (list-length shifts))) ;indice do shift
        (setf random-shift (nth random-shift-index shifts)) ;estrutura shift
        
        (setf shift-tasks (shift-tasks random-shift)) ;lista de shifts da estrutura shit escolhida
        (setf random-task-index (random (list-length shift-tasks))) ;
        (setf random-task (nth random-task-index shift-tasks))
        
        ;Change to-assign-tasks slot
        (setf new-incomplete-state (copy-structure state))
        (setf (state-to-assign-tasks new-incomplete-state) (push random-task (state-to-assign-tasks new-incomplete-state)))
        
        ;Remove random task from the random shift
        (setf shift-tasks (remove-nth random-task-index shift-tasks)) ;eliminada tarefa da lista de tasks do shift
        (setf (shift-tasks random-shift) shift-tasks) ;atualizar o random-shift com a nova lista de tasks
        (setf (shift-time-used random-shift) (calc-shift-time shift-tasks)) ;atualizar o random-shift com recalculado time-used
        (setf (nth random-shift-index shifts) random-shift) ;atualizar a lista de shifts com o alterado random_shift
        
        ;Change shifts slot
        (setf (state-shifts new-incomplete-state) shifts)

        ;(print new-incomplete-state)
        
        ;Generate new possible sucessors, from the created "new-incomplete-state"
        (setf sucessores (problema-gera-sucessores problema new-incomplete-state))

        ;Select a random sucessor
        (nth (random (list-length sucessores)) sucessores)
          
    )
)       

    ;;DONE: remove one random task from one random shift
    ;;DONE: call gera-sucessores to the resulting state, with the removed task in the toAssign list
    ;;DONE: select a random state from the gera-sucessores call result
    
    ;TODO: Testar neighbour e fazer a funcao do simulated_annealing

