;3 Определите функционал (APL-APPLY f x),
;который применяет каждую функцию fi списка
;(f1 f2 ... fn)
;к соответствующему элементу списка
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


;7. Определите фильтр (УДАЛйЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список
;все элементы, которые не обладают свойством, наличие которого проверяет
;предикат пред.



(defun delet(pred lst)
    (mapcan (lambda (x) (if (funcall pred x) (list x) nil)) lst)
)


;;; Test 1
(write-line "Test 1")
(princ " >> (1 2 a)")
(print (delet 'numberp '(1 2 a)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (a a)")
(print (delet 'numberp '(a a)))
(write-line "")

(write-line "")
