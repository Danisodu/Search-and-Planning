(load "G008.LISP")
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

        ;(print "sucessores")
        ;(print sucessores)
        
        (cond  ((eq sucessores nil) nil) ;nao e' solucao e e´ folha retornamos nil
               (t (progn 
                        (if (> (- (get-universal-time) tempo-inicio) tempo-max-primitido) ;if timeout approaching stop
                                (return-from 1-samp-aux -1)
                        )
                        (setf nextStateToExpandIndex (random (list-length sucessores)))
                        (setf nextStateToExpand (nth nextStateToExpandIndex sucessores))
                        ;(print "next: ")
                        ;(print nextStateToExpand)
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
            (print "runtime (s):") (print (- (get-universal-time) tempo-inicio))
            melhor-resultado
    )    

)


;;--------------------------------
;;--------TESTES------------------
;;--------------------------------



 
 (setf toAssignTasks '((L1 L5 552 647) (L1 L10 886 953) (L1 L10 1406 1473) (L1 L3 1264 1315) (L1 L1 1116 1145) (L1 L5 972 1067) (L1 L13 1018 1041) (L1 L2 394 455) (L1 L7 916 1003) (L1 L12 1090 1144) (L1 L2 424 485) (L1 L10 446 513) (L1 L1 1094 1120) (L1 L11 1024 1115) (L1 L7 502 597) (L1 L12 630 709) (L1 L7 702 797) (L1 L12 550 629) (L1 L7 802 897) (L1 L8 338 401) (L1 L10 866 933) (L1 L2 314 375) (L1 L6 726 793) (L1 L10 996 1063) (L1 L5 432 527) (L1 L2 654 715) (L1 L6 1246 1313) (L1 L5 1132 1227) (L1 L8 818 881) (L1 L8 638 701) (L1 L10 1086 1153) (L1 L1 12 52) (L1 L8 858 921) (L1 L10 816 883) (L1 L10 666 733) (L1 L10 1426 1493) (L1 L11 444 535) (L1 L5 672 767) (L2 L2 381 405) (L8 L12 375 429) (L1 L10 746 813) (L1 L4 862 917) (L1 L11 534 625) (L1 L11 1194 1285) (L1 L1 34 60) (L1 L4 582 637) (L1 L2 504 565) (L1 L4 1202 1257) (L1 L13 798 821) (L1 L6 1326 1393) (L1 L1 6 31) (L1 L10 416 483) (L1 L13 1118 1141) (L1 L11 404 495) (L1 L9 1208 1311) (L1 L11 584 675) (L1 L7 1316 1403) (L1 L11 574 665) (L1 L7 522 617) (L1 L2 1104 1165) (L1 L8 1158 1221) (L2 L2 421 445) (L1 L7 436 523) (L1 L7 442 537) (L1 L11 844 935) (L1 L5 1332 1427) (L1 L8 1138 1201) (L1 L5 630 709) (L1 L6 926 993) (L1 L4 1142 1197) (L1 L11 454 545) (L1 L10 586 653) (L1 L7 356 443) (L1 L6 906 973) (L1 L4 542 597) (L1 L8 1038 1101) (L1 L4 482 537) (L1 L5 470 549) (L1 L7 1296 1383) (L1 L10 1096 1163) (L1 L8 1078 1141) (L1 L2 524 585) (L1 L9 508 611) (L1 L4 702 757) (L1 L7 616 703) (L1 L10 786 853) (L1 L1 38 65) (L1 L2 1024 1085) (L1 L7 956 1043) (L1 L4 562 617) (L1 L10 506 573) (L1 L11 374 465) (L1 L12 810 889) (L1 L2 614 675) (L1 L11 944 1035) (L1 L5 1032 1127) (L1 L13 778 801) (L1 L10 826 893) (L1 L6 1066 1133) (L1 L1 1154 1180) (L5 L9 348 491) (L1 L11 294 385) (L1 L8 418 481) (L2 L2 468 487) (L1 L6 1146 1213) (L1 L2 544 605) (L1 L3 1304 1355) (L1 L5 430 509) (L1 L10 1056 1123) (L1 L9 448 551) (L1 L10 1326 1393) (L1 L5 1112 1207) (L1 L2 764 825) (L1 L6 446 513) (L1 L11 634 725) (L1 L5 650 729) (L1 L5 990 1069) (L1 L7 496 583) (L1 L2 664 725) (L1 L6 406 473) (L1 L7 1356 1444) (L1 L9 708 811) (L1 L6 786 853) (L1 L7 376 463) (L1 L12 1070 1124) (L1 L12 650 729) (L1 L9 608 711) (L1 L11 504 595) (L1 L9 1128 1231) (L1 L10 1386 1453) (L1 L10 596 663) (L1 L2 594 655) (L1 L2 874 935) (L1 L2 754 815) (L1 L10 556 623) (L1 L6 846 913) (L1 L10 696 763) (L1 L8 498 561) (L1 L13 898 921) (L1 L2 1434 1506) (L1 L10 1036 1103) (L1 L7 762 857) (L1 L6 546 613) (L11 L11 396 435) (L1 L8 1238 1301) (L1 L5 652 747) (L1 L2 1314 1385) (L1 L12 1050 1129) (L1 L10 806 873) (L1 L12 470 549) (L1 L10 936 1003) (L1 L5 832 927) (L1 L9 1368 1471) (L1 L10 726 793) (L1 L5 412 507) (L8 L9 310 391) (L1 L13 958 981) (L1 L10 896 963) (L1 L9 908 1011) (L1 L13 578 601) (L1 L9 1408 1512) (L1 L9 808 911) (L1 L6 806 873) (L1 L8 518 581) (L1 L2 934 995) (L1 L2 674 735) (L5 L9 328 471) (L1 L11 1254 1345) (L1 L8 798 861) (L1 L4 642 697) (L1 L9 1308 1411) (L1 L11 1044 1135) (L1 L4 662 717) (L1 L15 852 908) (L1 L5 912 1007) (L1 L5 1070 1149) (L1 L10 616 683) (L1 L4 442 497) (L1 L6 426 493) (L1 L7 1022 1117) (L1 L4 922 977) (L1 L10 606 673) (L1 L7 316 403) (L1 L5 712 807) (L1 L10 436 503) (L1 L10 836 903) (L1 L4 1342 1397) (L1 L7 922 1017) (L1 L8 438 501) (L1 L7 382 477) (L5 L9 308 451) (L1 L4 422 477) (L1 L1 46 75) (L1 L2 1334 1405) (L1 L7 962 1057) (L1 L5 592 687) (L1 L5 692 787) (L1 L11 974 1065) (L1 L8 378 441) (L1 L2 374 435) (L1 L7 1416 1514) (L1 L9 928 1031) (L1 L9 488 591) (L1 L4 382 437) (L1 L5 810 889) (L1 L5 350 429) (L1 L5 1212 1307) (L1 L9 648 751) (L1 L6 586 653) (L1 L7 1176 1263) (L1 L5 530 609) (L1 L7 1376 1464) (L1 L5 410 489) (L1 L5 1010 1089) (L1 L5 872 967) (L8 L4 294 377) (L1 L4 822 877) (L1 L10 536 603) (L1 L3 1364 1415) (L1 L10 456 523) (L1 L5 532 627) (L9 L5 325 467) (L2 L10 321 413) (L1 L7 942 1037) (L1 L11 1084 1179) (L1 L12 770 849) (L1 L5 452 547) (L1 L6 1426 1494) (L1 L8 978 1041) (L1 L2 1414 1485) (L1 L2 574 635) (L1 L8 758 821) (L1 L3 1184 1235) (L1 L8 958 1021) (L1 L8 1398 1461) (L1 L5 1172 1267) (L1 L9 668 771) (L1 L11 524 615) (L1 L8 18 82) (L4 L8 336 421) (L1 L10 376 443) (L1 L8 1178 1241) (L1 L10 776 843) (L1 L1 1134 1160) (L1 L2 644 705) (L1 L5 710 789) (L1 L1 1074 1100) (L1 L4 942 997) (L1 L11 834 925) (L1 L8 578 641) (L1 L9 1108 1211) (L1 L11 924 1015) (L2 L3 448 512) (L1 L7 1062 1130) (L1 L4 402 457) (L1 L4 762 817) (L1 L12 590 669) (L1 L7 1396 1484) (L1 L11 514 605) (L8 L4 314 397) (L1 L10 676 743) (L1 L6 306 373) (L1 L2 1194 1265) (L1 L4 1122 1177) (L1 L9 568 671) (L1 L13 458 481) (L1 L2 1374 1445) (L1 L10 636 703) (L1 L8 1358 1421) (L1 L10 546 613) (L1 L8 1318 1381) (L1 L10 916 983) (L1 L13 698 721) (L1 L7 656 743) (L1 L5 1232 1327) (L1 L2 534 595) (L2 L10 341 433) (L1 L3 756 823) (L1 L5 632 727) (L1 L3 500 540) (L1 L9 1328 1431) (L1 L10 1286 1353) (L1 L5 930 1009) (L1 L11 734 825) (L1 L7 1096 1183) (L1 L11 1314 1405) (L1 L6 486 553) (L1 L10 646 713) (L1 L5 970 1049) (L1 L10 406 473) (L1 L13 858 881) (L1 L2 854 915) (L8 L12 395 449) (L1 L2 1234 1305) (L1 L8 878 941) (L1 L12 570 649) (L1 L9 1008 1111) (L1 L5 932 1027) (L1 L6 1126 1193) (L1 L12 910 989) (L1 L11 814 905) (L1 L5 792 887) (L1 L13 638 661) (L1 L2 554 615) (L1 L13 518 541) (L1 L1 1114 1140) (L1 L11 654 745) (L1 L12 410 489) (L1 L2 584 645) (L1 L9 868 971) (L1 L9 408 511) (L1 L2 1274 1345) (L1 L5 512 607) (L1 L3 1244 1295) (L1 L2 744 805) (L1 L10 706 773) (L1 L9 528 631) (L1 L4 802 857) (L1 L5 952 1047) (L1 L5 590 669) (L1 L7 542 637) (L1 L10 796 863) (L1 L7 422 517) (L1 L6 386 453) (L1 L6 1116 1203) (L1 L6 746 813) (L1 L2 864 925) (L1 L8 318 381) (L1 L7 1016 1103) (L1 L2 694 755) (L1 L2 724 785) (L1 L4 1362 1417) (L1 L1 26 55) (L1 L12 510 589) (L1 L5 390 469) (L1 L10 1006 1073) (L1 L1 46 71) (L1 L5 730 809) (L1 L2 1354 1425) (L1 L2 1044 1105) (L1 L6 706 773) (L1 L10 1046 1113) (L1 L2 434 495) (L1 L7 1276 1363) (L1 L12 790 869) (L1 L5 610 689) (L1 L8 558 621) (L1 L1 14 40) (L10 L2 304 395) (L1 L2 784 845) (L1 L11 684 775) (L1 L7 676 763) (L1 L6 506 573) (L1 L10 976 1043) (L1 L4 1182 1237) (L1 L12 730 809) (L1 L11 1394 1485) (L1 L9 1288 1391) (L1 L10 736 803) (L1 L7 1216 1303) (L1 L8 678 741) (L1 L2 1174 1245) (L1 L9 688 791) (L1 L3 1124 1175) (L1 L5 1392 1487) (L1 L8 658 721) (L1 L11 394 485) (L1 L11 904 995) (L1 L7 876 963) (L1 L10 1066 1133) (L1 L10 716 783) (L1 L11 414 505) (L5 L9 288 431) (L1 L12 530 609) (L1 L13 918 941) (L1 L11 1414 1506) (L1 L9 768 871) (L1 L7 1042 1110) (L1 L11 614 705) (L6 L11 314 475) (L1 L12 850 929) (L1 L12 450 529) (L1 L4 742 797) (L1 L1 26 51) (L1 L10 1106 1173) (L1 L8 718 781) (L1 L10 496 563) (L1 L4 1402 1457) (L1 L8 398 461) (L1 L9 988 1091) (L1 L10 766 833) (L1 L2 774 835) (L1 L7 816 903) (L1 L11 1174 1265) (L1 L9 1048 1151) (L1 L8 1018 1081) (L1 L2 824 885) (L1 L4 622 677) (L1 L5 572 667) (L1 L12 610 689) (L1 L9 788 891) (L1 L8 1258 1321) (L1 L5 1030 1109) (L1 L7 822 917) (L1 L13 1058 1081) (L1 L11 1374 1465) (L1 L7 936 1023) (L1 L5 732 827) (L1 L10 486 553) (L1 L11 464 555) (L1 L9 1268 1371) (L1 L6 1166 1233) (L1 L6 1046 1113) (L1 L7 602 697) (L1 L5 392 487) (L1 L1 32 72) (L1 L2 1124 1185) (L1 L10 686 753) (L1 L4 1022 1077) (L1 L6 1306 1373) (L1 L6 966 1033) (L1 L5 1312 1407) (L1 L12 490 569) (L1 L5 332 427) (L1 L8 1278 1341) (L1 L12 930 1009) (L1 L7 402 497) (L1 L11 794 885) (L1 L9 748 851) (L1 L2 474 535) (L1 L2 1254 1325) (L1 L6 666 733) (L1 L13 618 641) (L1 L11 1154 1245) (L1 L2 844 905) (L1 L13 838 861) (L1 L15 632 688) (L1 L6 1086 1153) (L1 L5 1110 1189) (L1 L2 804 865) (L1 L12 390 469) (L1 L7 562 657) (L1 L7 1336 1423) (L1 L9 968 1071) (L1 L2 704 765) (L2 L10 281 373) (L1 L10 1026 1093) (L1 L8 738 801) (L1 L10 1186 1253) (L1 L15 1172 1228) (L1 L5 850 929) (L1 L9 1428 1510) (L1 L9 1188 1291) (L1 L10 526 593) (L1 L2 924 985) (L1 L11 764 855) (L1 L3 460 500) (L1 L2 974 1035) (L6 L11 289 405) (L1 L9 1068 1171) (L1 L7 836 923) (L1 L7 742 837) (L1 L12 710 789) (L8 L12 355 409) (L1 L1 1432 1472) (L1 L7 682 777) (L1 L10 1146 1213) (L1 L12 1110 1164) (L1 L2 714 775) (L1 L8 1058 1121) (L1 L13 998 1021) (L1 L3 448 492) (L1 L10 946 1013) (L1 L3 1224 1275) (L1 L10 1206 1273) (L1 L6 1106 1173) (L1 L2 1034 1095) (L1 L7 856 943) (L1 L11 804 895) (L6 L14 408 460) (L1 L2 1084 1145) (L1 L13 498 521) (L1 L10 906 973) (L1 L8 698 761) (L1 L11 744 835) (L1 L2 494 555) (L1 L4 1422 1477) (L1 L4 1162 1217) (L1 L10 956 1023) (L1 L5 890 969) (L1 L3 1344 1395) (L1 L13 538 561) (L1 L6 1346 1413) (L1 L7 1196 1283) (L1 L6 986 1053) (L1 L10 576 643) (L1 L7 902 997) (L1 L13 658 681) (L1 L4 1302 1357) (L1 L10 566 633) (L1 L4 1002 1057) (L1 L2 1054 1115) (L1 L9 888 991) (L2 L10 361 453) (L2 L2 428 447) (L1 L2 1144 1205) (L1 L11 994 1085) (L1 L5 910 989) (L1 L5 352 447) (L1 L10 966 1033) (L1 L4 362 417) (L1 L2 1164 1225) (L1 L11 554 645) (L1 L10 466 533) (L1 L12 890 969) (L6 L14 448 500) (L1 L4 1322 1377) (L1 L2 1214 1285) (L1 L8 1098 1161) (L1 L5 670 749) (L2 L10 301 393) (L1 L10 1246 1313) (L1 L4 902 957) (L1 L4 1382 1437) (L1 L7 1236 1323) (L1 L11 894 985) (L1 L2 514 575) (L1 L5 750 829) (L1 L11 364 455) (L1 L8 1218 1281) (L1 L5 1090 1169) (L1 L6 466 533) (L1 L5 1292 1387) (L1 L11 1104 1210) (L1 L11 1004 1095) (L1 L12 830 909) (L1 L6 686 753) (L1 L2 444 505) (L1 L8 938 1001) (L1 L8 298 361) (L1 L12 430 509) (L1 L11 1274 1365) (L6 L11 309 425) (L1 L11 1064 1155) (L1 L2 834 895) (L1 L9 828 931) (L1 L11 1034 1125) (L1 L4 2 57) (L1 L8 1298 1361) (L1 L3 1284 1335) (L1 L6 826 893) (L1 L5 1012 1107) (L1 L5 450 529) (L1 L13 478 501) (L1 L9 1168 1271) (L1 L5 772 867) (L1 L9 728 831) (L1 L8 998 1061) (L1 L3 1134 1225) (L1 L10 986 1053) (L1 L8 838 901) (L1 L8 1438 1502) (L1 L10 1366 1433) (L1 L2 894 955) (L1 L5 550 629) (L1 L7 362 457) (L1 L11 1334 1425) (L1 L4 842 897) (L1 L8 1378 1441) (L1 L4 1242 1297) (L1 L4 722 777) (L1 L5 312 407) (L1 L4 22 77) (L1 L11 1354 1445) (L1 L6 1226 1293) (L1 L10 1016 1083) (L1 L11 714 805) (L1 L11 754 845) (L1 L2 634 695) (L1 L3 480 520) (L1 L7 482 577) (L1 L13 758 781) (L1 L5 1272 1367) (L1 L9 308 411) (L1 L7 882 977) (L1 L10 626 693) (L1 L6 566 633) (L1 L2 1394 1465) (L1 L6 1206 1273) (L1 L9 548 651) (L1 L10 6 74) (L1 L5 892 987) (L1 L11 594 685) (L1 L11 544 635) (L1 L7 336 423) (L1 L1 54 80) (L1 L8 778 841) (L1 L5 812 907) (L1 L12 870 949) (L1 L6 1266 1333) (L1 L2 944 1005) (L1 L5 790 869) (L1 L5 690 769) (L1 L6 1286 1353) (L1 L7 996 1083) (L1 L2 954 1015) (L1 L6 646 713) (L1 L7 536 623) (L1 L5 1050 1129) (L1 L2 564 625) (L1 L2 404 465) (L1 L7 456 543) (L6 L14 428 480) (L1 L7 662 757) (L1 L2 884 945) (L1 L7 896 983) (L1 L4 782 837) (L1 L4 502 557) (L1 L10 426 493) (L1 L10 846 913) (L1 L10 656 723) (L1 L8 478 541) (L1 L4 462 517) (L1 L3 1324 1375) (L1 L9 948 1051) (L1 L11 854 945) (L1 L6 1026 1093) (L1 L2 734 795) (L1 L9 1148 1251) (L1 L4 1082 1137) (L1 L11 724 815) (L1 L7 596 683) (L10 L2 324 415) (L1 L5 490 569) (L1 L9 1028 1131) (L1 L11 874 965) (L1 L5 1192 1287) (L1 L10 1126 1193) (L1 L5 1072 1167) (L1 L13 1038 1061) (L1 L12 990 1069) (L1 L7 1082 1150) (L1 L2 964 1025) (L1 L11 914 1005) (L1 L7 862 957) (L1 L4 1102 1157) (L1 L2 604 665) (L1 L9 1388 1492) (L6 L11 329 445) (L1 L7 1036 1123) (L1 L8 1418 1482) (L1 L9 1088 1191) (L1 L5 830 909) (L1 L12 670 749) (L1 L7 556 643) (L1 L7 516 603) (L2 L2 401 425) (L1 L11 1234 1325) (L1 L5 852 947) (L1 L5 992 1087) (L1 L11 424 515) (L1 L2 1004 1065) (L1 L8 598 661) (L1 L7 462 557) (L1 L5 1052 1147) (L1 L11 1014 1105) (L1 L9 1348 1451) (L1 L10 1266 1333) (L1 L5 612 707) (L1 L2 1064 1125) (L1 L2 914 975) (L1 L7 476 563) (L1 L3 488 532) (L1 L5 570 649) (L1 L13 558 581) (L1 L11 474 565) (L1 L5 370 449) (L1 L7 1136 1223) (L1 L5 1412 1507) (L1 L7 982 1077) (L1 L2 904 965) (L1 L4 1222 1277) (L1 L4 1042 1097) (L1 L12 750 829) (L1 L2 484 545) (L1 L11 934 1025) (L1 L12 1010 1089) (L1 L4 882 937) (L1 L9 588 691) (L1 L11 564 655) (L1 L6 946 1013) (L1 L2 684 745) (L1 L10 1166 1233)))
 
 
 (setf problem (cria-problema (create-state '() toAssignTasks)
                           (list #'sucessors)
                          :objectivo? #'is-goal
                         :estado= #'equality))
 
 
 ;(print (sondagem-iterativa problem (get-universal-time)))
(setf res (multiplas-sondagens-iterativas problem))
(print "multiplas-sondagens-iterativas res:")
(print res)
(print "custo: ")
(print (calculate-state-cost res))
 
 
