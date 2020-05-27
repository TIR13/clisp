# clisp
## Матричный язык Lisp. Этот язык способный прозводить операции над матрицами. 

В версии 1.0 можно найти транспонированную матрицу, минимальный/максимальный элемент матрицы, определитель квадратной матрицы, сложение и умножение матриц.

Подключить язык можно, с помощью
```
  (require "путь_к_файлу/lang.lisp")
```

## Примеры
Найти максимальный/минимальный элемент:

```
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

```
Вывод

```

M1 = ((3 5 1) (4 5 6) (7 8 9))
M2 = ((1 2 4) (4 5 6) (7 8 9))
((30 39 51) (66 81 100) (102 126 157)) 
((4 7 5) (8 10 12) (14 16 18)) 
9 
1 
((1 4 7) (2 5 8) (4 6 9)) 
M2 = ((4 5 1) (4 5 6) (7 8 9))
((40 47 49) (78 93 88) (123 147 136)) 
((5 7 5) (8 10 12) (14 16 18)) 
9 
1 
((4 4 7) (5 5 8) (1 6 9)) 
M2 = ((4 5 1) (4 5 6) (7 8 9))
```
