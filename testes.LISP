(load "C:/Users/X541U/OneDrive/IST/Search-and-Planning/G008.lisp")

 (let ((state (list (create-state '() '((L2 L1 1 25) (L10 L1 4 33) (L1 L2 14 40) (L1 L11 14 55) (L4 L1 16 37) (L2 L1 21 45) (L1 L10 26 55) (L1 L9 28 72) (L1 L2 34 60) (L1 L10 46 75) (L10 L1 364 393) (L1 L1 394 465) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 565))))) )


(loop while (> (length state) 0) do
      (setf state (sucessors (nth 0 state)))
      (print state)))

;(sucessors (create-state (list (create-shift '((L1 L1 0 0)))) '((L1 L1 0 4)(L1 L2 5 10))))
(defun printStatesShifts (lista)
    (print lista)
    (loop for state in lista do
        (print "State")
        (loop for shift in (state-shifts state) do
            (print shift)
        )))

(let ( (res1)
       (res2)
       (res3)
       (res4)
       (res5)
       (res6)
       (res7)
       (res8))

  (setf res1 (can-lunch (create-shift '((L1 L1 0 20) (L2 L3 40 480))))
        res2 (can-lunch (create-shift '((L2 L1 40 50)(L1 L2 50 241))))
        res3 (can-lunch (create-shift '((L1 L1 40 50)(L1 L2 50 241))))
        res4 (can-lunch (create-shift '((L1 L1 0 20)(L1 L2 60 480))))
        res5 (sucessors (create-state (list (create-shift '((L1 L1 40 60))))
                                      '((L1 L1 0 40))))
        res6 (sucessors (create-state (list (create-shift '((L1 L1 40 60))))
                                      '((L1 L2 0 40))))
        res7 (sucessors (create-state (list (create-shift '((L1 L1 40 300)(L2 L3 400 450))))
                                      '((L2 L2 300 400)))))

        (print (eq res1 nil))
        (print (eq res2 nil))
        (print res3)
        (print res4)
        (print res5)
        (print res6)
        (print res7)
)
