;1. Определите FUNCALL через функционал APPLY.
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


;3 Определите функционал (APL-APPLY f x),
;который применяет каждую функцию fi списка
;(f1 f2 ... fn)
;к соответствующему элементу списка
;x = (x1 x2 ... xn)

(defun apl(f x)
      (cond
         ((null f) nil)
          (t(cons (apply (car f) (car x)) (apl (cdr f) (cdr x))))
       )
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





; 5. Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен, 
;когда, являющейся функциональным аргументом предикат пред истинен 
;хотя бы для одного элемента списка список

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



;7. Определите фильтр (УДАЛйЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список
;все элементы, которые не обладают свойством, наличие которого проверяет
;предикат пред.



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




;9. Напишите генератор порождения чисел Фибоначчи:
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





;11. Определите фукнционал МНОГОФУН, который использует функции, являющиеся
;аргументами, по следующей схеме:


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
;;; Определите функцию, которая возвращает своё определение
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
