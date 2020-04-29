; 5. ќпределите функциональный предикат (Ќ≈ ќ“ќ–џй пред список), который истинен, 
;когда, €вл€ющейс€ функциональным аргументом предикат пред истинен 
;хот€ бы дл€ одного элемента списка список

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

;7. ќпределите фильтр (”ƒјЋй№-≈—Ћ»-Ќ≈ пред список), удал€ющий из списка список
;все элементы, которые не обладают свойством, наличие которого провер€ет
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




;11. ќпределите фукнционал ћЌќ√ќ‘”Ќ, который использует функции, €вл€ющиес€
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