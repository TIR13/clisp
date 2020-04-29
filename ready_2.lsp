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