; 11
; Определите функцию, осуществляющую разделение
;исходного списка на два подсписка. 
; В первый из них должно попасть указанное количество элементов
; с начала списка, во второй — оставшиеся элементы.
; (9 1 8 2) 2 -> ((9 1) (8 2))


(defun split (lst k)
    (cond
        ((null lst) nil)
            (t
              ((lambda (first res)
                  ((lambda (x y)
                       (cond
                           ((> k 0)
                                (cons (cons first x) (cdr y)))
                            (t  
                                (list x (cons first (cadr y))))
                        )
                     )(car (split res (- k 1))) (split res (- k 1)))
                )(car lst)(cdr lst))
         )
    )
)

(write-line "Задача 11")
;;; Test 1
(write-line "Test 1")
(princ " >> (1 2 3) 2)")
(print (split '(1 2 3) 2)
)
(write-line "")
;;; Test 2
(write-line "Test 2")
(princ " >> (1 2 3) 3)")
(print (split '(1 2 3) 3))
(write-line "")

(write-line "")



; 12
;Определите функцию, заменяющую 
;в исходном списке два подряд
;идущих одинаковых элемента одним.
;(1 1 1) -> (1 1)

(defun unique1 (lst) 
    ((lambda (first res)
        (cond 
            ((null lst) lst) 
            ((eq first (cadr lst)) (cons first (unique1 (cddr lst)))) 
            (t (cons first (unique1 res)))
        )
     )(car lst)(cdr lst))
) 
(write-line "Задача 12")
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


; 13. Определите функцию, удаляющую 
; в исходном списке все повторные вхождения элементов.
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
        (t 
              ((lambda (first res)
                  ((lambda (dfn)     
                      (cond 
                          ((pr first res) dfn) 
                          (t (cons first dfn))
                      )
                  )(unique res))
              )(car lst)(cdr lst))
        )
    )   
)  

(write-line "Задача 13")
;;; Test 1
(write-line "Test 1")
(princ " >> (1 1 3)")
(print (unique '(1 1 3)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (1 1 1 2 2)")
(print (unique '(1 1 1 2 2)))
(write-line "")
(write-line "")



; 19
; Определите функцию (ЛУКОВИЦА n), 
; строящую N-уровневый вложенный список, 
;элементом которого на самом глубоком уровне является N.
; 4 -> ((((4))))

(defun lis(n)
   (t_lis n n)
)
(defun t_lis(n k)
    (cond
        ((eq k 0) n)
        (t ( cons (t_lis n (- k 1)) nil))
     ))
(write-line "Задача 19")
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


; 31. Определите функцию (ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), 
; которая возвращает первый элемент, 
; входящий в оба списка х и у, в противном случае NIL.
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
    ((lambda (first res)
        (cond 
            ((null l1) nil) 
            ((eq first (p first l2)) first) 
            (t( lis res l2)) 
        )
    )(car l1)(cdr l1))     
) 
(write-line "Задача 31")
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


; 41. Реализовать генератор деревьев, чтобы выдаваемые
; им деревья имели количество вершин, точно соответствующее 
; числу, указанному в его первом аргументе.

(defun der1 (n k)
	
	(cond ((<= n k) nil)
		(t
        ((lambda (first)
            (list k (der1 n first) (der1 n (+ first 1))))
        (+ (* k 2) 1)))
	)

)
(defun der(n)
        (der1 n 0)
)
(write-line "Задача 41")


;;; Test 1
(write-line "Test 1")
(princ " >> 5")
(print (der 5))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> 3")
(print (der 3))
(write-line "")
(write-line "")



;47. Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА,
;которая удаляет все свойства символа.
;
;

(defun del-all (symb)
	( (lambda (s-list)
		(if (null s-list)
			nil
			(cons (remprop symb (car s-list)) (del-all symb))
		)) (symbol-plist symb)
	)
)
(write-line "Задача 47")
;;; Test 1
(setf (get 'cars 'color) 'red)
(setf (get 'cars 'тип) 'седан)
(print "Test 1")
(print (symbol-plist 'cars))
(del-all 'cars)
(print (symbol-plist 'cars))
(write-line "")

;;; Test 2
(setf (get 'человек 'возраст) '30)


(print "Test 2")
(print (symbol-plist 'человек))
(del-all 'человек)
(print (symbol-plist 'человек))
(write-line "")



;48. Функция GET возвращает в качестве результата NIL
;в том случае, если у символа нет данного свойства, либо
;если значением этого свойства является NIL.
;Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в
;списке свойств. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство),
;который проверяет, обладает ли символ данным свойством.

(defun prov (symb sv)
	(if(null (get symb sv)) nil t)
)
(write-line "Задача 48")
;;; Test 1
(setf (get 'cars 'color) 'red)
(setf (get 'cars 'тип) 'седан)


;;; Test 1
(print "Test 1")
(print (symbol-plist 'cars))
(print "(prov cars color)")
(print (prov 'cars 'color))
(write-line "")

;;; Test 2
(print "Test 2")
(print (symbol-plist 'cars))
(print "(prov cars col)")
(print (prov 'cars 'col))
(write-line "")








; 5
;Определите функцию, которая увеличивает
;элементы исходного списка на единицу.
; (9 1 8 2) -> (10 2 9 3)
(defun inc(lst) 
	(cond 
        ((null lst) nil)
		(t(cons (+ (car lst) 1) (inc (cdr lst))))
	)
)
(write-line "Задача 5")
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
; Определите функцию, удаляющую из исходного 
; списка элементы с четными номерами.
;  (9 1 8 2) 2 -> (9 8)

(defun del (lst) 
    (cond 
        ((null lst) lst) 
        (t (cons (car lst) (del (cddr lst))))
     )
) 
(write-line "Задача 7")
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

