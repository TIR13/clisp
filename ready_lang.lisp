(require "/lang.lisp")


(print "Transp: ((1 2 3) (4 5 6) (7 8 9))")
(print (Transp '((1 2 3) (4 5 6) (7 8 9))))

(write-line "")
(print "Maxx: ((1 2 3) (4 5 6) (7 8 9))")
(print (Maxx '((1 2 3) (4 5 6) (7 8 9))))

(write-line "")
(print "Minn: ((1 2 3) (4 5 6) (7 8 9))")
(print (Minn '((1 2 3) (4 5 6) (7 8 9))))

(write-line "")
(print "Determ: ((3 5 1) (4 5 6) (7 8 9))")
(print (Determ '((3 5 1) (4 5 6) (7 8 9))))

(write-line "")
(print "Sum: ((1 2 3) (4 5 6) (7 8 9))")
(print (Sum '((1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9))))

(write-line "")
(print "Multiplay: ((1 2 3) (4 5 6) (7 8 9))")
(print (Multiplay '((1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9))))



