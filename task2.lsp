;1. ���������� FUNCALL ����� ���������� APPLY.
;

(defun func (f &rest args)
    (apply f args)
)


;;; Test 1
(write-line "Test 1")
(princ " >> (1 2 3 4)")
(print (func '+  1 2 3 4))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (6 2 3)")
(print (func '*  6 2 3))
(write-line "")

(write-line "")


;3 ���������� ���������� (APL-APPLY f x),
;������� ��������� ������ ������� fi ������
;(f1 f2 ... fn)
;� ���������������� �������� ������
;x = (x1 x2 ... xn)

(defun apl(f x)
	(mapcar 'apply f x)
)


;;; Test 1
(write-line "Test 1")
(princ " >> (1 2 3 4)")
(print (apl '(+ *) '((1 2 3) (1 2 3))))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (6 2 3)")
(print (apl '(*) '((1 2 5))))
(write-line "")

(write-line "")





; 5. ���������� �������������� �������� (��������� ���� ������), ������� �������, 
;�����, ���������� �������������� ���������� �������� ���� ������� 
;���� �� ��� ������ �������� ������ ������

(defun prov(lst) 
    (not (null (mapcan 
		#'(lambda (x) (if (funcall 'numberp x) (list t))) lst))
     )
)

;;; Test 1
(write-line "Test 1")
(princ " >> (1 2 a)")
(print (prov '(1 2 a)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (a a)")
(print (prov '(a a)))
(write-line "")

(write-line "")



;7. ���������� ������ (������-����-�� ���� ������), ��������� �� ������ ������
;��� ��������, ������� �� �������� ���������, ������� �������� ���������
;�������� ����.



(defun delet(lst)
    (mapcan (lambda (x) (if (numberp x) (list x) nil)) lst)
)


;;; Test 1
(write-line "Test 1")
(princ " >> (1 2 a)")
(print (delet '(1 2 a)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (a a)")
(print (delet '(a a)))
(write-line "")

(write-line "")




;9. �������� ��������� ���������� ����� ���������:
;0, 1, 1, 2, 3, 5, ...

(defun fib ()
	(let ((p1 0) (p2 1))
		(lambda ()
			(setq temp p1 p1 (+ p2 p1) p2 temp))
	)
)
(setq c1 (fib))

;;; Test 1
(write-line "Test 1")

(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))





;11. ���������� ���������� ��������, ������� ���������� �������, ����������
;�����������, �� ��������� �����:


(defun mnog (func lst)
    (mapcar (lambda (f) (apply f lst)) func)
)

;;; Test 1
(write-line "Test 1")
(princ " >> '(+ -) '(1 2 3)")
(print (mnog '(+ -) '(1 2 3)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> '(* /) '(1 2 3)")
(print (mnog '(* /) '(1 2 3)))
(write-line "")

(write-line "")



;;; #13
;;; ���������� �������, ������� ���������� ��� �����������
(setq getme
    '(
        (lambda (x)
            (list x (list 'quote x))
        )
        '(lambda (x)
            (list x (list 'quote x))
        )
    )
)


;;; Test #1
(write-line "Test 1")
(princ " eval function")
(print (eval getme))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " recursively eval function 3 times")
(print (eval (eval (eval getme))))
(write-line "")