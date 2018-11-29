;(load "procura.lisp")

;;; 1-samp-aux: estado x problema -> estado
;;  recebe um estado e um problema e expande a partir do estado dado de forma aleatoria, ate encontrar um estado fronteira
;;  devolve esse estado fronteira (ou seja estado sem sucessores)
(defun 1-samp-aux (estado problema)
    (let ((gera-sucessores (problema-gera-sucessores problema))
          (sucessores)
          (nextStateToExpandIndex)
          (nextStateToExpand)
          ;(objectivo? (problema-objectivo? problema))
          )
          
          
        (setf sucessores (funcall gera-sucessores estado))
        
        (cond  ((eq sucessores nil) (estado)) ;Se nao tiver sucessores e´ fronteira e fazemos return do estado
               (t (progn 
                        (setf nextStateToExpandIndex (random (list-length sucessores)))
                        (setf nextStateToExpand (nth nextStateToExpandIndex sucessores))
                        (1-samp-aux nextStateToExpand problema)))
        ) 
    )
)  
;;; 1-samp: problema -> estado
;;  faz uma sondagem iterativa a comecar no estado inicial e a escolher sucessores aleatorios ate um estado fronteira
;;  devolve o estado fronteira 
(defun 1-samp (problema)
        (let ((estado-inicial (problema-estado-inicial problema)))  
             
             (1-samp-aux estado-inicial problema)))

 
;;; sondagem-iterativa: problema -> estado (solucao)
;;  faz sondagens 1-samp ate que uma das sondagens resulte num estado solucao 
(defun sondagem-iterativa (problema)
    (let ((estado-solucao nil)
        (1-samp-res)
        (objectivo? (problema-objectivo? problema)))
    
         
        (loop (while (not estado-solucao))
            (setf 1-samp-res (1-samp problema))
            (if (funcall objectivo? 1-samp-res) (setf estado-solucao 1-samp-res))
        )
        
        estado-solucao
    )
)