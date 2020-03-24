; 5
;���������� �������, ������� �����������
;�������� ��������� ������ �� �������.
; (9 1 8 2) -> (10 2 9 3)
(defun inc(lst) 
	(cond 
        ((null lst) nil)
		(t(cons (+ (car lst) 1) (inc (cdr lst))))
	)
)
;;; Test 1
(write-line "Test 1")
(princ " >> (5 2 3)")
(print (inc '(5 2 3)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (9 1 8 2)")
(print (inc '(9 1 8 2)))
(write-line "")

(write-line "")

; 7
; ���������� �������, ��������� �� ��������� 
; ������ �������� � ������� ��������.
;  (9 1 8 2) 2 -> (9 8)

(defun del (lst) 
    (cond 
        ((null lst) lst) 
        (t (cons (car lst) (del (cddr lst))))
     )
) 

;;; Test 1
(write-line "Test 1")
(princ " >> (a b c d e)")
(print(del '(a b c d e)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (a b c d)")
(print(del '(a b c d)))
(write-line "")

(write-line "")

; 11
; ���������� �������, �������������� ����������
;��������� ������ �� ��� ���������. 
; � ������ �� ��� ������ ������� ��������� ���������� ���������
; � ������ ������, �� ������ � ���������� ��������.
; (9 1 8 2) 2 -> ((9 1) (8 2))



(defun split (lst k)
    ((lambda (first restl)
        (cond 
            ((null lst) nil)
            ( (> k 0) (cons (cons first (car (split restl (- k 1)))) (cdr (split restl (- k 1)))))
            (t (list (car (split restl (- k 1))) (cons first (cadr (split restl (- k 1))))))
        )
    )(car lst)(cdr lst)
))


;;; Test 1
(write-line "Test 1")
(princ " >> (1 2 3) 2)")
(print (split '(1 2 3) 2))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (1 2 3) 3)")
(print (split '(1 2 3) 3))
(write-line "")

(write-line "")


; 12
;���������� �������, ���������� 
;� �������� ������ ��� ������
;������ ���������� �������� �����.
;(1 1 1) -> (1 1)

(defun unique1 (lst) 
    (cond 
        ((null lst) lst) 
        ((eq (car lst) (cadr lst)) (cons (car lst) (unique1 (cddr lst)))) 
        (t (cons (car lst) (unique1 (cdr lst))))
    )
) 

;;; Test 1
(write-line "Test 1")
(princ " >> (1 1 1)")
(print (unique1 '(1 1 1)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (1)")
(print (unique1 '(1)))
(write-line "")

(write-line "")


; 13. ���������� �������, ��������� 
; � �������� ������ ��� ��������� ��������� ���������.
; (1 1 1) -> (1)


(defun pr(a lst)
    (cond
        ((null lst) nil)
        ((eq a (car lst)) t)
        (t(pr a (cdr lst)))
     )
)

(defun unique (lst) 
    (cond 
        ((null lst) nil)
        ((pr (car lst) (cdr lst)) (unique (cdr lst))) 
        (t(cons (car lst) (unique (cdr lst))))
    ) 
)  


;;; Test 1
(write-line "Test 1")
(princ " >> (1 1 3)")
(print (unique '(1 1 2)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (1 1 1 2 2)")
(print (unique '(1 1 1 2 2)))
(write-line "")
(write-line "")



; 19
; ���������� ������� (�������� n), 
; �������� N-��������� ��������� ������, 
;��������� �������� �� ����� �������� ������ �������� N.
; 4 -> ((((4))))

(defun lis(n)
   (t_lis n n)
)
(defun t_lis(n k)
    (cond
        ((eq k 0) n)
        (t ( cons (t_lis n (- k 1)) nil))
     ))
;;; Test 1
(write-line "Test 1")
(princ " >> 3 ")
(print (lis '3))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> 0")
(print (lis '0))
(write-line "")
(write-line "")


; 31. ���������� ������� (������-����������� � �), 
; ������� ���������� ������ �������, 
; �������� � ��� ������ � � �, � ��������� ������ NIL.
; (1 3 3) (2 6 0) -> nil
; (1 3 3) (1 6 0) -> 1
(defun p(a l) 
    (cond 
        ((null l) nil) 
        ((eq a (car l)) a) 
        (t( p a (cdr l))) 
    ) 
) 

(defun lis (l1 l2) 
    (cond 
        ((null l1) nil) 
        ((eq (car l1) (p (car l1) l2)) (car l1)) 
        (t( lis (cdr l1) l2)) 
    ) 
) 

;;; Test 1
(write-line "Test 1")
(princ " >> (1 3 3) '(2 6 0)")
(print (lis '(1 3 3) '(2 6 0)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (1 3 3) (2 3 1)")
(print (lis '(1 3 3) '(2 3 1)))
(write-line "")

