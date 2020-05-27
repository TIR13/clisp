
--12. Определите функцию, разбивающую список (a b с d...) на пары ((а b) (с d)...).

main = do
    print $ zip ["1","2","3","4","5"] ["a","5","5","5","5"]  


--24. Написать функция Разность, формирующая разность двух множеств.

delete :: Eq a => a -> [a] -> [a]
delete  = deleteBy (==)
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x [] = []
deleteBy eq x (y:ys) = if x `eq` y then ys else y : deleteBy eq x ys

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) =  foldl (flip delete)
main = do
    print $ [1, 2, 3] \\ [4]
    print $ [1, 2, 3] \\ [3]

