(load "C:/Users/X541U/OneDrive/IST/Search-and-Planning/G008.lisp")

 (let ((state (list (create-state '() '((L2 L1 1 25) (L10 L1 4 33) (L1 L2 14 40) (L1 L11 14 55) (L4 L1 16 37) (L2 L1 21 45) (L1 L10 26 55) (L1 L9 28 72) (L1 L2 34 60) (L1 L10 46 75) (L10 L1 364 393) (L1 L1 394 465) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 565))))) )
(faz-afetacao '((L1 L1 0 240)(L10 L1 240 280)(L1 L1 280 361)) "ILDS")

(faz-afetacao '((L2 L1 1 25) (L10 L1 4 33) (L1 L2 14 40) (L1 L11 14 55) (L4 L1 16 37) (L2 L1 21 45) (L1 L10 26 55) (L1 L9 28 72) (L1 L2 34 60) (L1 L10 46 75) (L10 L1 364 393) (L1 L1 394 465) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 565) (L1 L1 512 607) (L1 L1 552 647) (L1 L1 566 633) (L1 L1 594 665) (L1 L1 608 711) (L1 L1 666 733) (L1 L1 684 735) (L1 L1 698 761) (L1 L1 722 777) (L1 L1 736 823) (L1 L1 752 847) (L1 L1 762 817) (L1 L1 776 863) (L1 L1 778 841) (L1 L1 802 857) (L1 L1 808 911) (L1 L1 814 905) (L1 L1 848 951) (L1 L1 856 943) (L1 L1 864 915) (L1 L1 866 933) (L1 L1 886 973) (L1 L1 906 953) (L1 L1 906 973) (L1 L1 934 1005) (L1 L1 944 995) (L1 L1 946 1013) (L1 L1 954 1045) (L1 L1 974 1045) (L1 L1 994 1085) (L1 L1 996 1083) (L1 L1 1018 1081) (L1 L1 1046 1113) (L1 L1 1058 1121) (L1 L1 1078 1141) (L1 L1 1082 1137) (L1 L1 1084 1135) (L1 L1 1086 1153) (L1 L1 1114 1185) (L1 L1 1122 1177) (L1 L1 1144 1195) (L1 L1 1148 1251) (L1 L1 1152 1247) (L1 L1 1168 1271) (L1 L1 1178 1241) (L1 L1 1194 1265) (L1 L1 1196 1283) (L1 L1 1224 1275) (L1 L1 1232 1327) (L1 L1 1234 1325) (L1 L1 1248 1351) (L1 L1 1272 1367) (L1 L1 1276 1363) (L1 L1 1284 1335) (L1 L1 1286 1353) (L1 L1 1318 1381) (L1 L1 1328 1431) (L1 L1 1336 1423) (L1 L1 1358 1421) (L1 L1 1382 1437) (L1 L1 1386 1453) (L1 L1 1386 1453) (L1 L2 1394 1420) (L1 L10 1406 1435) (L1 L2 1414 1440) (L1 L4 1422 1444) (L1 L5 1432 1472)) "ILDS")

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
