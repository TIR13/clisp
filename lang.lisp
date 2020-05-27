
; минимальный элемент в матрице
(defun Minn (matr)
    ((lambda (lst)(min_s lst))(mapcar #'min_s matr))
)

;максимальный элемент в матрице
(defun Maxx (matr)
    ((lambda (lst)(max_s lst ))(mapcar #'max_s matr ))
)

;определитель квадратной матрицы
(defun Determ (matr)
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

; Сложение
(defun Sum(matr1 matr2)
    (cond
        ((and (null matr1) (null matr2)) nil)
        (t
            (cons
                (mapcar '+ (car matr1) (car matr2))
                (Sum (cdr matr1) (cdr matr2))
            )
        )
    )
)

 ; матричное умножение
(defun Multiplay (matr1 matr2)
    (cond
        ((null matr1) nil)
        (t (cons (row_col (car matr1) matr2) (Multiplay (cdr matr1) matr2)))
    )
)

;транспонирование
(defun Transp (matr)
    (apply 'mapcar 'list matr)
)



; максимум по строке
(defun max_s (matr)
   (cond    
        ((null (cdr matr)) (car matr))
        ((> (car matr) (max_s (cdr matr))) (car matr))
        (t (max_s (cdr matr)))
   )
)

; минимум по строке
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
; строка на все столбцы
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



;возвращает список чисел от 1 до n
(defun seq (n)
    (cond
        ((= n 0) nil)
        (t (append (seq (- n 1)) (list n))) 
    )
)
;удалить элемент списка
(defun del_el (lst n)
    (cond 
        ((= n 1) (cdr lst))
        (t (cons (car lst) (del_el (cdr lst) (- n 1))))
    )
) 

 ;минора матрицы
(defun minor (matr m)
    (mapcar #'(lambda (x) (del_el x m)) (del_el matr 1))
)
 
;следующий столбец
(defun other_column(matr)
    (cond
        ((null matr) nil)
            (t (cons (cdar matr) (other_column (cdr matr)))
        )
    )
)
;строка на столбец
(defun column(matr)
    (cond
        ((null matr) nil)
        (t (cons (caar matr) (column (cdr matr))))
    )
)



(defun out (whole)
    ((lambda (first second rest ost)
        (cond
            ((null whole) 'Matrix)
            ((and 
                (eq second '=)
                (eq (cadr ost) '=)
            )
            (and 
                (setq m1 rest n1 first)(print first)(princ '=)(princ rest)
                (setq m2 (caddr ost) n2 (car ost))(print (car ost))(princ '=)(princ m2)(out (cdddr ost))))
            ((eq second '=) (and (setq m1 m2 n1 n2)(setq m2 rest n2 first)(print first)(princ '=)(princ rest)(out ost)))
            ((eq first '*) (and (print (multiplay m1 m2)) (out (cdr whole))))
            ((eq first '+) (and (print (Sum m1 m2)) (out (cdr whole))))
            ((eq first 'max) (and (print (Maxx m2)) (out (cdr whole))))
            ((eq first 'min) (and (print (Minn m2)) (out (cdr whole))))
            ((eq first 't) (and (print (Transp m2)) (out (cdr whole))))
            ((eq first '@) (and (print n2)(princ '=)(princ m2) (out (cdr whole))))
            (t (print whole))
     )
             
     
   )(car whole)(cadr whole)(caddr whole)(cdddr whole)
)
    )


(defmacro Matrix (&rest tokens )
	`(let
		(
                (whole ',tokens)
        	(val Nil)
		)
      (out whole)

	)
)

(Matrix 
    M1 = ((3 5 1) (4 5 6) (7 8 9))
    M2 = ((1 2 4) (4 5 6) (7 8 9)) 
    *
    +
    max 
    min 
    t
    M1 = ((4 5 1) (4 5 6) (7 8 9))
    *
    +
    max 
    min 
    t
    @
)



