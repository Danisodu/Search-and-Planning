(load "G008.LISP")
;;; 1-samp-aux: estado x problema -> estado
;;  recebe um estado e um problema e expande a partir do estado dado de forma aleatoria
;;  se encontrar um estado objectivo devolve esse estado, se nao chega ate uma folha nao objectivo e retorna nil
(defun 1-samp-aux (estado problema)
    (let ((gera-sucessores (problema-gera-sucessores problema))
          (sucessores)
          (nextStateToExpandIndex)
          (nextStateToExpand)
          (objectivo? (problema-objectivo? problema))
          )
          
            
        (if (funcall objectivo? estado) (return-from-l-samp-aux estado))    
        (setf sucessores (funcall gera-sucessores estado))
        
        (cond  ((eq sucessores nil) nil) ;nao e' solucao e e´ folha retornamos nil
               (t (progn 
                        (setf nextStateToExpandIndex (random (list-length sucessores)))
                        (setf nextStateToExpand (nth nextStateToExpandIndex sucessores))
                        (1-samp-aux nextStateToExpand problema)))
        ) 
    )
)  
;;; 1-samp: problema -> estado ou nil
;;  faz uma sondagem iterativa a comecar no estado inicial e a escolher sucessores aleatorios ate um estado fronteira
;;  devolve um estado objectivo ou nil caso nao tenha sido encontrado
(defun 1-samp (problema)
        (let ((estado-inicial (problema-estado-inicial problema)))  
             
             (1-samp-aux estado-inicial problema)))

 
;;; sondagem-iterativa: problema -> estado (solucao)
;;  faz sondagens 1-samp ate que uma das sondagens resulte num estado solucao 
(defun sondagem-iterativa (problema)
    (let ((1-samp-res nil))
    
        (loop (while (not 1-samp-res))
            (setf 1-samp-res (1-samp problema)))
        
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
           )
           (loop (while (< (- (get-universal-time) tempo-inicio) tempo-max-primitido))
           
                (setf resultado-sondagem (sondagem-iterativa problema))
                
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