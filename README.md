# clisp

## 5. Определите функцию, которая увеличивает элементы исходного списка на единицу.

```
(defun inc(lst) 
	(cond 
        ((null lst) nil)
		(t(cons (+ (car lst) 1) (inc (cdr lst))))
	)
)

(print (inc '(5 2 3)))
(6 3 4)

(print (inc '(1)))
(2) 

(print (inc nil))
NIL
 
```

## 7. Определите функцию, удаляющую из исходного списка элементы с четными номерами.

```
(defun del (lst) 
    (cond 
        ((< (length lst) 2) lst) 
        (t (cons (car lst) (del (cddr lst))))
     )
) 

(print(del '(a b c d e f)))
(A C E) 

(print(del '(a)))
(A) 

(print(del '(nil)))
(NIL) 
```

## 11. Определите функцию, осуществляющую разделение исходного списка на два подсписка. В первый из них должно попасть указанное количество элементов
с начала списка, во второй — оставшиеся элементы.

```
(defun add-el (lst el)
    (cond 
        ((null lst) (list el))
        (t (cons (car lst) (add-el (cdr lst) el)))
    )
)

(defun f (lst m &optional l1 l2) 
    (cond 
        ((null lst) (if (and (> (length l2) 0) (= (length l1) 0)) (list (list l1) l2) (if (= (length l2) 0) (list l1 (list l2)) (list l1 l2)) ))
        ((> m 0) (f (cdr lst) (- m 1) (add-el l1 (car lst)) l2)) 
        (t (f (cdr lst) m l1 (add-el l2 (car lst)))) 
    ) 
) 

(print (f '(1 2 6 9 7 4) 2))
((1 2) (6 9 7 4))

(print (f '(1 2 6 9 7 4) 0))
((NIL) (1 2 6 9 7 4))

(print (f '(1 2 6 9 7 4) -5))
((NIL) (1 2 6 9 7 4)) 

(print (f '(nil) 2))
((NIL) (NIL)) 
 
```

## 12. Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним.

```
(defun unique1 (lst) 
    (cond 
        ((< (length lst) 2) lst) 
        ((eq (car lst) (cadr lst)) (cons (car lst) (unique1 (cddr lst)))) 
        (t (cons (car lst) (unique1 (cdr lst))))
    )
) 

(print (unique1 '(1 1 1)))
(1 1) 

(print (unique1 '(1 2 1)))
(1 2 1) 

(print (unique1 '(1)))
(1)

```

## 13. Определите функцию, удаляющую в исходном списке все повторные вхождения элементов.

```
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

(print (unique '(1 1 2)))
(1 2)

(print (unique '(1 1 1)))
(1) 

(print (unique '(3 1 2)))
(3 1 2)

```

## 19. Определите функцию (ЛУКОВИЦА n), строящую N-уровневый вложенный список,элементом которого на самом глубоком уровне является N.

```
(defun lis (n &optional (s n) ) 
    (cond 
        ((eq n 0) s)
        (t(lis (- n 1) (list s)))
    ) 
) 

(print (lis 0))
0

(print (lis 4))
((((4))))
```

## 31. Определите функцию (ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), которая возвращает первый элемент, входящий в оба списка х и у, в противном случае NIL.

```
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

(print (lis '(1 3 3) '(2 6 0)))
NIL 

(print (lis '(1 3 3) '(1 6 0)))
1

(print (lis '(1 3 3) '(2 3 1)))
1
```
