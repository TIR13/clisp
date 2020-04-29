;48. Функция GET возвращает в качестве результата NIL   
;в том случае, если у символа нет данного свойства, либо
;если значением этого свойства является NIL.
;Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в
;списке свойств. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство),
;который проверяет, обладает ли символ данным свойством.

(defun prov (symb sv)
	(prov_sv sv (symbol-plist symb))
)



(defun prov_sv(sv list)
  (cond
      ((null list) nil)
      ((equal sv (car list)) T)
      (t (prov_sv sv (cddr list)))
  )
)
(write-line "Задача 48")
;;; Test 1
(setf (get 'cars 'color) 'nil)
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
