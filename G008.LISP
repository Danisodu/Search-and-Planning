;;;; Author(s): Group 8


(in-package :user)

;-------------------------------------------------------------------------
;----------------------------------CONSTANTS------------------------------
;-------------------------------------------------------------------------

(defconstant MAX-SHIFT 480)
(defconstant MAX-TIME-TIL-BREAK 240)
(defconstant DRIVE-DURATION 40)
(defconstant EAT-DURATION 40)
(defconstant MAX-SUCESSORS 3)
(defconstant MAX-TIME 270)
(defconstant MAX-SPACE 262144)

;-------------------------------------------------------------------------
;-----------------------------------STRUCTS-------------------------------
;-------------------------------------------------------------------------


(defstruct shift
  tasks
  time-used
  meal-taken)

(defstruct state
  shifts
  to-assign-tasks
  heuristic-value)


;-------------------------------------------------------------------------
;---------------------------------HEURISTICS------------------------------
;-------------------------------------------------------------------------


(defun number-to-assign-tasks (state)
  (length (state-to-assign-tasks state)))

(defun number-hours-to-assign (state)
  (let((sum-all-hours 0)
       (to-assign-tasks (state-to-assign-tasks state))
       (task-duration))

    (loop for task in to-assign-tasks do
          (setf task-duration (- (nth 3 task) (nth 2 task)))
          (incf sum-all-hours task-duration))

    sum-all-hours))


;-------------------------------------------------------------------------
;------------------------------ALGORITHMS---------------------------------
;-------------------------------------------------------------------------

(defun copy-shift (shift-old)
  (let ((shift-new (copy-structure shift-old))
        (shift-tasks-copy))

    (setf shift-tasks-copy (copy-list (shift-tasks shift-old)))

    (setf (shift-tasks shift-new) shift-tasks-copy)

    shift-new))

(defun copy-state (state-old)
  (let ((state-new (copy-structure state-old))
        (shifts-new (mapcar #'copy-structure (state-shifts state-old)))
        (to-assign-tasks-new (copy-list (state-to-assign-tasks state-old))))

        ;copy each shift to new from the old
    (loop for i from 0 to (- (list-length shifts-new) 1) do
          (setf (nth i shifts-new) (copy-shift (nth i shifts-new))))

    (setf (state-shifts state-new) shifts-new)

    ;copy to to-assign-tasks
    (setf (state-to-assign-tasks state-new) to-assign-tasks-new)

    state-new))

;-----------------------------------------
;------------------A*---------------------
;-----------------------------------------

;;;re-defining calcula-novo-g from procura.lisp
(defun calcula-novo-g (estado no-pai custo-transicao)
  "Calcula o novo valor de g para estado, sendo no-pai o seu pai."
    (declare (ignore no-pai) (ignore custo-transicao))

    (calculate-state-cost estado))


;------------------------------------------
;----------------ILDS----------------------
;------------------------------------------
(defun sort-by-time(tasks)
  (sort tasks #'< :key #'fourth)
  (sort tasks #'< :key #'third))

(defun sort-by-heuristic-value (states)
  (loop for state in states do
        (setf (state-heuristic-value state) (+ (calculate-state-cost state)
                                               (number-hours-to-assign state))))

  (sort states #'< :key #'state-heuristic-value))

(defun ILDS-iteration (state discrepancies depth time)
  (let ((best-discrepancy-res)
        (best-discrepancy-val most-positive-fixnum)
        (res)
        (sucss (sort-by-heuristic-value (sucessors state))))

     (if (>= (- (get-universal-time) time)
             MAX-TIME)
        (return-from ilds-iteration nil))

    (if (is-goal state)
        (if (= discrepancies 0)
              (return-from ILDS-iteration state)
          (return-from ilds-iteration nil)))

    (if (and (> discrepancies 0)
             (> (length sucss) 1))
        (progn
          (setf res (ILDS-iteration (nth 1 sucss) (1- discrepancies) (1- depth) time))

          (if (not (null res))
              (if (< (state-heuristic-value res)
                     best-discrepancy-val)
                  (setf best-discrepancy-res res
                        best-discrepancy-val (state-heuristic-value res))))))

    (if (> depth discrepancies)
        (progn
          (setf res (ILDS-iteration (car sucss) discrepancies (1- depth) time))

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
         (depth (length (state-to-assign-tasks initial-state)))
         (best-solution initial-state)
         (init-time (get-universal-time)))

    (loop while (and (<= discrepancies depth)
                     (< (- (get-universal-time) init-time)
                         MAX-TIME)) do

          (setf res (ILDS-iteration initial-state discrepancies depth init-time))

          (if (and (not (null res))
                   (< (state-heuristic-value res)
                      (state-heuristic-value best-solution)))
                (setf best-solution res))

          (incf discrepancies))

    best-solution))


;-----------------------------------------
;----------Sondagem Iterativa-------------
;-----------------------------------------
;;; 1-samp-aux: estado x problema -> estado
;;  recebe um estado e um problema e expande a partir do estado dado de forma aleatoria
;;  se encontrar um estado objectivo devolve esse estado, se nao chega ate uma folha nao objectivo e retorna nil
(defun 1-samp-aux (estado problema tempo-inicio)
    (let (
          (sucessores)
          (nextStateToExpandIndex)
          (nextStateToExpand)
          (objectivo? (problema-objectivo? problema))
          (gera-sucessores (car (problema-operadores problema)))
          (tempo-max-primitido (* 4.5 60))

          )


        (if (funcall objectivo? estado)  (return-from 1-samp-aux estado))
        (setf sucessores (funcall gera-sucessores  estado))


        (cond  ((eq sucessores nil) nil) ;nao e' solucao e e´ folha retornamos nil
               (t (progn
                        (if (> (- (get-universal-time) tempo-inicio) tempo-max-primitido) ;if timeout approaching stop
                                (return-from 1-samp-aux -1)
                        )
                        (setf nextStateToExpandIndex (random (list-length sucessores)))
                        (setf nextStateToExpand (nth nextStateToExpandIndex sucessores))
                        (1-samp-aux nextStateToExpand problema tempo-inicio)))
        )
    )
)
;;; 1-samp: problema -> estado ou nil
;;  faz uma sondagem iterativa a comecar no estado inicial e a escolher sucessores aleatorios ate um estado fronteira
;;  devolve um estado objectivo ou nil caso nao tenha sido encontrado
(defun 1-samp (problema tempo-inicio)
        (let ((estado-inicial (problema-estado-inicial problema)))
             (1-samp-aux estado-inicial problema tempo-inicio)))


;;; sondagem-iterativa: problema -> estado (solucao)
;;  faz sondagens 1-samp ate que uma das sondagens resulte num estado solucao
(defun sondagem-iterativa (problema tempo-inicio)

    (let ((1-samp-res nil))

        (loop while (not 1-samp-res) do
            (setf 1-samp-res (1-samp problema tempo-inicio))
        )

        1-samp-res
    )
)

;;;multiplas-sondagens-iterativas: problema -> estado (solucao)
;; recebe um problema e devolve a melhor solucao de todas as sondagens-iterativas feitas ate timeout
(defun multiplas-sondagens-iterativas (problema)
    (let ((tempo-inicio (get-universal-time))
           (tempo-max-primitido (* 4.5 60)) ;max e 5min, estamos a fazer para 4m30s
           (melhor-resultado nil)
           (melhor-resultado-custo)
           (resultado-sondagem nil)
           (resultado-sondagem-custo)
           (stop nil)
           )
           (loop named looper while (or (< (- (get-universal-time) tempo-inicio) tempo-max-primitido) (not stop)) do

                (setf resultado-sondagem (sondagem-iterativa problema tempo-inicio))

                ;avoid starting something that can not finish at time
                (if (equal resultado-sondagem -1) (return-from looper))

                (cond ((equal melhor-resultado nil)
                            (setf melhor-resultado resultado-sondagem)
                            (setf melhor-resultado-custo (calculate-state-cost resultado-sondagem))
                        )
                        (t
                            (setf resultado-sondagem-custo  (calculate-state-cost resultado-sondagem))
                            (if (< resultado-sondagem-custo melhor-resultado-custo)
                                (progn
                                    (setf melhor-resultado resultado-sondagem)
                                    (setf melhor-resultado-custo (calculate-state-cost resultado-sondagem))))
                        )
                )

            )
            melhor-resultado
    )

)


;-----------------------------------------
;------------Simulated Annealing----------
;-----------------------------------------


(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))


;;; create-random-initial-solution: problema -> estado (solucao)
;;  usa uma sondagem iterativa para gerar uma solucao valida aleatoria
(defun create-random-initial-solution (problem tempo-inicio)
    (sondagem-iterativa problem tempo-inicio)
)


;;; neighbour: state -> state
;;  Receives a state and returns a state similar to the one received but with a small change
;;  The change in this case is one random task is moved to a random shift

(defun neighbour(state problema)
    (let ((shifts (mapcar #'copy-shift (state-shifts state)))
        (shift-tasks)
        (random-shift-index)
        (random-shift)
        (random-task-index)
        (random-task)
        (new-incomplete-state)
        (sucessores)
        (gera-sucessores (car (problema-operadores problema)))
        )

        (setf random-shift-index (random (list-length shifts)))
        (setf random-shift (copy-shift (nth random-shift-index shifts)))

        (setf shift-tasks (copy-list (shift-tasks random-shift)))
        (setf random-task-index  (- (list-length shift-tasks) 1))
        (setf random-task (nth random-task-index shift-tasks))
        (setf new-incomplete-state (copy-state state))

        (setf (state-to-assign-tasks new-incomplete-state) (push random-task (state-to-assign-tasks new-incomplete-state)))


        (setf shift-tasks (remove-nth random-task-index shift-tasks))

         (cond ((equal shift-tasks nil)
                    (setf shifts (remove-nth random-shift-index shifts))
                )
                (t
                    (setf (shift-tasks random-shift) shift-tasks)
                    (setf (shift-time-used random-shift) (calc-shift-time shift-tasks))
                    (setf (nth random-shift-index shifts) random-shift)
                )
         )

        (setf (state-shifts new-incomplete-state) shifts)

        ;Generate new possible sucessors, from the created "new-incomplete-state"
        (setf sucessores (funcall gera-sucessores new-incomplete-state))

        ;Select a random sucessor
        (nth (random (list-length sucessores)) sucessores)
    )
)

(defun acceptance-probability (cost-change temp)
      (exp ( / (* cost-change -1) temp))
)



(defun random-prob ()
    ( / (+ (random 100) 1) 100.0)
)


(defun simulated_annealing (problema)
    (let (
        (initial-solution)
        (temp 100000) ;min 100000 para dar acceptance-probability=1 na primeira iteracao com cost-change maximo
        (tempo-inicio (get-universal-time))
        (tempo-max-primitido (* 4.5 60))
        (temp-min 10) ;minimo e' 10 para nao dar overflow em acceptance-probability
        (alpha   0.95)
        (x-current)
        (x-new)
        (cost-change)
        (acc_prob)
        (iterations 0)
        (nr-iter-per-temp 3)
        (temp-it-i 0 )
        )


        (setf initial-solution (create-random-initial-solution problema tempo-inicio))

        (setf x-current initial-solution)
        (loop while (and (< (- (get-universal-time) tempo-inicio) tempo-max-primitido) (> temp temp-min)) do
            (setf temp-it-i 0)
            (loop while (and (< (- (get-universal-time) tempo-inicio) tempo-max-primitido) (< temp-it-i nr-iter-per-temp)) do
                (setf x-new (neighbour x-current problema))
                (setf cost-change (- (calculate-state-cost x-new) (calculate-state-cost x-current)))
                (cond ((<=  cost-change 0)
                            (setf x-current x-new)
                       )
                       (t
                            (setf acc_prob (acceptance-probability cost-change temp))

                            (if (> acc_prob (random-prob))
                                    (progn
                                        (setf x-current x-new)
                                    )
                             )
                        )

                )

                (incf iterations)
                (incf temp-it-i)

            )

            (setf temp (* temp alpha))

        )

        x-current
    )
)


;-------------------------------------------------------------------------
;---------------------------------FUNCS-----------------------------------
;-------------------------------------------------------------------------
(defun get-memory ()
  (let ((room (with-output-to-string (*standard-output*) (room))))

  (setf room (read-from-string room t nil :start 281))
  (setf room (string-trim "|" room))

  (multiple-value-bind (x) (parse-integer (string-trim "K" room))

  x)))

(defun final-list (state)
  (let((shifts (state-shifts state)))

  (loop for shift in shifts collect (shift-tasks shift))))

(defun add-drive-duration? (origin destination end-time)
  (if (not (eq origin destination))
      (setf end-time (+ end-time DRIVE-DURATION)))
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
               :time-used (calc-shift-time tasks)
               :meal-taken nil))

(defun create-state (shifts toAssignTasks)
  (make-state :shifts shifts
              :to-assign-tasks toAssignTasks
              :heuristic-value most-positive-fixnum))

(defun add-task-to-shift (task shift)
  (let ((new-tasks-list (nconc (shift-tasks shift) (list task))))

  (setf (shift-tasks shift) new-tasks-list
        (shift-time-used shift) (calc-shift-time new-tasks-list))))

(defun empty-space-two-tasks (t1 t2)
  (- (nth 2 t2)
     (add-drive-duration? (nth 1 t1)
                          (nth 0 t2)
                          (nth 3 t1))))

(defun when-to-lunch (tasks)
  (let ((tsks (copy-list tasks))
        (pos 0)
        (current)
        (next)
        (lunch-time))

    (loop while (< pos (1- (length tsks))) do
        (setf current (nth pos tsks)
              next (nth (1+ pos) tsks)
              pos (1+ pos))

        (if (>= (empty-space-two-tasks current
                                       next)
                EAT-DURATION)
            (if (> (- (nth 2 next) EAT-DURATION)
                   MAX-TIME-TIL-BREAK)
                (setf lunch-time (+ MAX-TIME-TIL-BREAK EAT-DURATION))
              (setf lunch-time (nth 2 next)))))

    lunch-time))

(defun verify-breaks (shift)
  (let* ((correct t)
         (lunch-time)
         (tasks (shift-tasks shift))
         (last-task (nth (1- (length tasks)) tasks))
         (drive-time 0))

    (if (not (eq (nth 1 last-task) 'L1))
        (setf drive-time drive-duration))

    (if (not (shift-meal-taken shift))
        (if (> (- (shift-time-used shift) drive-time)
                MAX-TIME-TIL-BREAK)
            (progn
              (setf lunch-time (when-to-lunch tasks))

              (if lunch-time
                  (setf (shift-meal-taken shift) lunch-time)
                (setf correct nil))))
      (if (> (- (nth 3 last-task) (shift-meal-taken shift))
                 MAX-TIME-TIL-BREAK)
             (setf correct nil)))

    (return-from verify-breaks correct)))

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
  (let* ((last-task (nth (1- (length tasks)) tasks))
         (first-task (nth 0 tasks))
         (shift-end (list 'L1 'L1 (+ (nth 2 first-task) MAX-SHIFT) nil)))

    (if (and (not (is-intersected last-task new-task))
             (not (is-intersected new-task shift-end)))
        (return-from fits-in-shift t))))


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
                    (< (length sucessors) MAX-SUCESSORS)) do

         (setf current-shift (copy-shift (nth pos shifts))
               stte (copy-state state2))

         (if (fits-in-shift next-task
                            (shift-tasks current-shift))
             (progn
               (add-task-to-shift next-task current-shift)

               (if (verify-breaks current-shift)
                   (progn
                     (setf (nth pos (state-shifts stte)) current-shift)
                     (setf sucessors (nconc sucessors (list stte)))))))
         (incf pos))

   (if (= (length sucessors) 0)
       (progn
         (setf new-shift (create-shift (list next-task)))
         (verify-breaks new-shift)
         (setf (state-shifts state2) (nconc (state-shifts state2) (list new-shift)))
         (setf sucessors (list state2))))

   sucessors))

(defun calculate-state-cost (state)
  (let ((shifts (state-shifts state))
        (shift-time)
        (sum-all-shifts 0))

    (dolist (shift shifts)
      (setf shift-time (shift-time-used shift))
      ;if less than 6h its considered 6h used
      (if (< shift-time (* 6 60)) (setf shift-time (* 6 60)))
      (incf sum-all-shifts shift-time))

    sum-all-shifts))

(defun is-goal (state)
  (eq (length (state-to-assign-tasks state)) 0))

(defun equality (s1 s2)
  (equalp s1 s2))

(defun faz-afetacao (toAssignTasks strategy)
 (let ((problem (cria-problema (create-state '() toAssignTasks)
                               (list #'sucessors)
                               :objectivo? #'is-goal
                               :estado= #'equality))
      (result))

       (cond ((string-equal strategy "melhor.abordagem")
                (setf result (simulated_annealing problem))
              )
             ((string-equal strategy "a*.melhor.heuristica")
                (setf (problema-heuristica problem) #'number-to-assign-tasks)
                (setf (problema-custo problem) #'calculate-state-cost)
                (setf result (last (car (procura problem "a*"))))
             )
             ((string-equal strategy "a*.melhor.heuristica.alternativa")
                (setf (problema-heuristica problem) #'number-hours-to-assign)
                (setf (problema-custo problem) #'calculate-state-cost)
                (setf result (last (car (procura problem "a*"))))
              )
             ((string-equal strategy "sondagem.iterativa")
                (setf result (multiplas-sondagens-iterativas problem))
              )
             ((string-equal strategy "ILDS")
                (setf result (ILDS problem))
              )
             ((string-equal strategy "abordagem.alternativa")
                (setf result (simulated_annealing problem))
              )
             (t
              (return-from faz-afetacao 'Wrong))
        )

        ;convert to external representation
        ;(setf result (final-list result))
        (print (length (final-list result)))
        (print result)
    )
)
