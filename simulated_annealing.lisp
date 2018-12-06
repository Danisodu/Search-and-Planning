(load "sondagem_iterativa.lisp")
;(setf *random-state* (make-random-state t))

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
    (let ((shifts (mapcar #'copy-structure (state-shifts state)))
        (shift-tasks)
        (random-shift-index)
        (random-shift)
        (random-task-index)
        (random-task)
        (new-incomplete-state)
        (sucessores)
        (gera-sucessores (car (problema-operadores problema)))
        )
        ;(print "initial state:")
        ;(print state)
        (setf random-shift-index (random (list-length shifts))) ;indice do shift
                ;(print "random-shift-index:")
                ;(print random-shift-index)
                ;(print "shifts:")
                ;(print shifts)
        (setf random-shift (copy-structure (nth random-shift-index shifts))) ;estrutura shift
                ;(print "random-shift:")
                ;(print random-shift)

        (setf shift-tasks (copy-list (shift-tasks random-shift))) ;lista de shifts da estrutura shit escolhida
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

       ; (print "new-incomplete-state:")
        ;(print new-incomplete-state)
        
        ;Generate new possible sucessors, from the created "new-incomplete-state"
        (setf sucessores (funcall gera-sucessores new-incomplete-state))

        ;Select a random sucessor
            ;(print "sucessores:")
            ;(print sucessores)

        (nth (random (list-length sucessores)) sucessores)
    )
)       

(defun acceptance-probability (cost-change temp)
    (print "cost-change, temp:") (print cost-change) (print temp)
    (if (<= cost-change 0) (print "deuuu merdaaa"))
      (exp ( / (* cost-change -1) temp))
)
              


(defun random-prob ()
    ( / (+ (random 100) 1) 100.0)
)
    
    
(defun simulated_annealing (problema)
    (let (
        (initial-solution (create-random-initial-solution problema))
        (temp 100000) ;min 100000 para dar acceptance-probability=1 na primeira iteracao com cost-change maximo
        (tempo-inicio (get-universal-time))
        (tempo-max-primitido (* 3 60))
        (temp-min 10) ;minimo e' 10 para nao dar overflow em acceptance-probability
        (alpha  0.99) 
        (x-current)
        (x-new) 
        (cost-change)
        (acc_prob)
        (iterations 0)
        (nr-iter-per-temp 10)
        (temp-it-i 0 )
        ;(cost-scale-down 1)
        )
        
        (print "inicial:") (print initial-solution)
        (print "custo inicial:") (print (calculate-state-cost initial-solution))
        
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

                            (print "acc_prob:") (print acc_prob)
                            (if (> acc_prob (random-prob))
                                    (progn 
                                        (setf x-current x-new)
                                    )
                             )
                        )
                            
                )
                                   
                (incf iterations)
                (incf temp-it-i)
                ;(print "temperatura")
                ;(print temp)
                ;(print "acabei")
            )
            
            (setf temp (* temp alpha))

        )
        (print "iterations:") (print iterations)
        (print "runtime (s):") (print (- (get-universal-time) tempo-inicio))
        x-current
    )
)



;;;-------------------------------------------------------
;;;------------------TESTING----------------------------
;;;-------------------------------------------------------


 ;(setf toAssignTasks '( (L2 L1 1 25) (L1 L2 34 60) (L5 L1 408 447) (L3 L2 500 600) (L1 L13 100 200) )) 
 (setf toAssignTasks '((L1 L2 1014 1085) (L1 L2 974 1045) (L1 L3 1024 1075) (L7 L7 656 697) (L1 L11 894 985) (L1 L1 626 655) (L1 L8 638 701) (L1 L4 922 977) (L1 L2 674 735) (L1 L2 1114 1185) (L1 L7 1196 1283) (L1 L1 26 51) (L1 L8 1238 1301) (L1 L11 1374 1465) (L1 L6 1306 1373) (L1 L11 1214 1305) (L1 L11 644 735) (L1 L1 924 950) (L1 L2 714 775) (L10 L10 664 693) (L2 L2 601 625) (L1 L6 666 733) (L1 L10 596 663) (L1 L11 1334 1425) (L1 L7 736 823) (L1 L7 742 837) (L1 L6 886 953) (L1 L10 876 943) (L1 L12 610 689) (L1 L1 902 945) (L1 L1 884 925) (L1 L5 872 967) (L1 L6 1426 1494) (L1 L8 978 1041) (L1 L10 566 633) (L1 L5 670 749) (L1 L10 446 513) (L1 L5 972 1067) (L1 L10 736 803) (L1 L8 678 741) (L1 L2 1274 1345) (L1 L5 730 809) (L1 L11 824 915) (L1 L4 1202 1257) (L1 L8 1058 1121) (L1 L4 1002 1057) (L1 L11 994 1085) (L1 L2 744 805) (L1 L5 492 587) (L1 L8 1118 1181) (L1 L11 374 465) (L5 L9 348 491) (L1 L10 1346 1413) (L1 L6 566 633) (L6 L11 329 445) (L1 L10 946 1013) (L1 L6 686 753) (L1 L10 1266 1333) (L1 L2 1054 1125) (L1 L4 522 577) (L1 L10 766 833) (L1 L12 890 944) (L8 L12 615 669) (L1 L9 908 1011) (L5 L9 328 471) (L1 L9 588 691) (L1 L7 1216 1303) (L1 L6 426 493) (L1 L2 1214 1285) (L1 L8 1198 1261) (L1 L5 1072 1167) (L1 L9 1028 1131) (L1 L5 752 847) (L1 L5 852 947) (L1 L11 634 725) (L1 L10 406 473) (L1 L2 374 445) (L1 L2 1314 1385) (L1 L2 994 1065) (L1 L2 534 595) (L1 L11 1174 1265) (L1 L7 696 783) (L1 L5 750 829) (L1 L5 732 827) (L1 L6 806 873) (L1 L9 308 411) (L1 L11 784 875) (L1 L4 1162 1217) (L1 L13 838 861) (L1 L5 1312 1407) (L1 L5 332 427) (L1 L3 1324 1375) (L1 L12 830 909) (L1 L5 1352 1447) (L1 L2 474 545) (L1 L11 494 585) (L1 L10 1386 1453) (L1 L4 622 677) (L1 L13 698 721) (L1 L8 1138 1201) (L1 L11 294 385) (L1 L2 454 525) (L1 L3 464 515) (L5 L9 288 431) (L1 L3 504 555) (L1 L11 734 825) (L1 L8 578 641) (L1 L4 942 997) (L1 L8 1098 1161) (L1 L9 968 1071) (L1 L8 398 461) (L1 L4 1122 1177) (L1 L4 1062 1117) (L1 L6 306 373) (L1 L9 568 671) (L1 L9 1168 1271) (L1 L8 998 1061) (L1 L10 986 1053) (L1 L5 772 867) (L1 L5 770 849) (L1 L10 676 743) (L1 L3 1264 1315) (L1 L10 526 593) (L1 L4 862 917) (L1 L11 714 805) (L1 L5 710 789) (L1 L2 394 465) (L1 L11 594 685) (L2 L2 581 605) (L1 L10 1066 1133) (L1 L7 796 883) (L1 L4 1262 1317) (L1 L7 1376 1464) (L1 L7 1176 1263) (L1 L10 1326 1393) (L1 L2 764 825) (L1 L11 844 935) (L1 L7 556 643) (L1 L2 1394 1465) (L1 L9 608 711) (L1 L10 466 533) (L1 L9 628 731) (L1 L5 612 707) (L1 L8 1178 1241) (L1 L4 462 517) (L1 L3 1164 1215) (L1 L6 386 453) (L1 L1 944 970) (L1 L12 850 904) (L1 L6 826 893) (L1 L10 606 673) (L1 L9 1308 1411) (L1 L2 834 896) (L1 L2 1154 1225) (L1 L11 474 565) (L1 L8 918 981) (L1 L5 810 889) (L1 L1 26 55) (L1 L2 514 575) (L1 L10 906 973) (L1 L1 14 40) (L1 L12 630 709)))
 
 (setf problem (cria-problema (create-state '() toAssignTasks)
                               (list #'sucessors)
                               :objectivo? #'is-goal
                               :estado= #'equality))
;(setf sol-inicial (sondagem-iterativa problem))
;(print sol-inicial)
;(print (neighbour sol-inicial problem))

(setf solution-found (simulated_annealing problem))
(print "solution found:") (print solution-found)
(print "custo final:") (print (calculate-state-cost solution-found))