(load "procura.lisp")
(load "G008.lisp")

(setf shitfTasks '((L2 L1 1 25)))
(setf toAssign '((L1 L3 26 30)))
(setf shift1 (create-shift shitfTasks)) 
(setf s1 (make-state :shifts (list shift1) :to-assign-tasks toAssign))

(print "Estado Inicial s1")
(print s1)
(print "sucessores s1")
(print (sucessors s1))