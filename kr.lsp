;2. Написатm генератор совершенных чисел.

(defun div(x &optional (y (floor x 2)))
	(cond 
        ((= y 0) nil)
	(t 
            ((lambda (res)
                (if (eq (mod x y) 0) (cons y res) res)
             )(div x (- y 1)))
        )
	)
)

(defun prov(x) 
	(if (= (apply '+ (div x)) x) t nil)
)

(defun gener ()
	(let ((x 5))
		(lambda() 
			(if (prov (setq x (+ x 1))) x (funcall s))
            
		)
	)
)

(setq s (gener))
(print (funcall s))
(print (funcall s))








;8. В заданном списке списков найти самый длинный подсписок. 
; (1 (1 4)) -> (1 4)


(defun max_lst(lst)
    ((lambda (first) 
             (mapcan #'
                     (lambda (res)
                              (if (eq (length res) first) (list res))
                     )lst
             )
    )(apply 'max (mapcar 'length lst)))
)

(print (max_lst '((4) (124 2) (1 3 3))))
