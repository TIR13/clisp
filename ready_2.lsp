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
