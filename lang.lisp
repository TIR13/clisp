
; ����������� ������� � �������
(defun minn (matr)
    ((lambda (lst)(min_s lst))(mapcar #'min_s matr))
)

;������������ ������� � �������
(defun maxx (matr)
    ((lambda (lst)(max_s lst ))(mapcar #'max_s matr ))
)

;������������ ���������� �������
(defun determ (matr)
    ((lambda (n)
        (cond (
            (= 2 n) (- (* (caar matr) (cadr (cadr matr))) (* (cadar matr) (caadr matr))))
            (t  
                ((lambda (stroka sign posl)
                    (apply '+ (mapcar #'(lambda (x y p) (* x y (determ (minor matr p)))) stroka sign posl))
                )(car matr) (seq_sig n) (seq n))
            )
        )
    )(length matr))
)

; ��������
(defun sum(matr1 matr2)
    (cond
        ((and (null matr1) (null matr2)) nil)
        (t
            (cons
                (mapcar '+ (car matr1) (car matr2))
                (add-matrix (cdr matr1) (cdr matr2))
            )
        )
    )
)

 ; ��������� ���������
(defun multiply (matr1 matr2)
    (cond
        ((null matr1) nil)
        (t (cons (row_col (car matr1) matr2) (multiply (cdr matr1) matr2)))
    )
)


; �������� �� ������
(defun max_s (matr)
   (cond    
        ((null (cdr matr)) (car matr))
        ((> (car matr) (max_s (cdr matr))) (car matr))
        (t (max_s (cdr matr)))
   )
)

; ������� �� ������
(defun min_s (matr)
   (cond    
        ((null (cdr matr)) (car matr))
        ((< (car matr) (min_s (cdr matr))) (car matr))
        (t (min_s (cdr matr)))
   )
)



(defun seq_sig (n &optional (res '(1)))
    (cond
        ((= n 1) res)
        (t (seq_sig (- n 1) (append res (list (- (car (last res)))))))
    )
)
; ������ �� ��� �������
(defun row_col (row matr)
    (cond
        ((null (car matr)) nil)
        (t
            (cons
                (apply '+ (mapcar '* row (column matr)))
                (row_col row (other_column matr))
            )
        )
    )
)



;���������� ������ ����� �� 1 �� n
(defun seq (n)
    (cond
        ((= n 0) nil)
        (t (append (seq (- n 1)) (list n))) 
    )
)
;������� ������� ������
(defun del_el (lst n)
    (cond 
        ((= n 1) (cdr lst))
        (t (cons (car lst) (del_el (cdr lst) (- n 1))))
    )
) 

 ;������ �������
(defun minor (matr m)
    (mapcar #'(lambda (x) (del_el x m)) (del_el matr 1))
)
 
;��������� �������
(defun other_column(matr)
    (cond
        ((null matr) nil)
            (t (cons (cdar matr) (other_column (cdr matr)))
        )
    )
)
;������ �� �������
(defun column(matr)
    (cond
        ((null matr) nil)
        (t (cons (caar matr) (column (cdr matr))))
    )
)


;����������������
(defun trans (matr)
    (apply 'mapcar 'list matr)
)



(print (trans '((1 2 3) (4 5 6) (7 8 9))))


