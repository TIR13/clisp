
--12. Определите функцию, разбивающий список (a b с d...) на пары ((а b) (с d)...).

main = do
    print $ zip ["1","2","3","4","5"] ["a","5","5","5","5"]  


--24. Написать функция Разность, формирующая разность двух множеств.

delete                  :: Eq a => a -> [a] -> [a]
delete                  =  deleteBy (==)

deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) =  foldl (flip delete)
main = do
    print $ [1, 2, 3] \\ [4]
    print $ [1, 2, 3] \\ [3]


---3Написать генератор деррева 

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
              
leaf x = Branch x Empty Empty

cbalTree :: Int -> [Tree Int]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch n left right |   
                                i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]
                                                         
main = do
print $ cbalTree 4









-------------------------------------------------------------------------------------------------------------------------------------

--2. Определите функци, функцию, заменяющую в исходном списке все вхождения заданного значения другим.

m lst from to = map 
    (\x -> if x == from
    then to else x) lst

main = do
print $ m [1,2,3,2,3] 2 5


-- 13. Реализовать на языке Haskell функцию, которая, чередуя элементы списков [a, b, ...] и [1, 2, ...], образует новыи? список [a, 1, b, 2, ...].

m [] x = if x == [] then x else m x []
m (x:xs) y = x : m y xs

main = do
print $ m ["1", "2", "3"] ["a", "b", "c"]

--26 Реализовать алгоритм сортировки слиянием.

sort [] = []
sort [l] = [l]
sort l =
    m (sort first) (sort second) where
        first = take ((length l) `div` 2) l
        second = drop ((length l) `div` 2) l

m f [] = f
m [] s = s
m (f:ft) (s:st)
  | f < s     = f:(m ft (s:st))
  | otherwise = s:(m (f:ft) st)


main = do
print $ sort  [6,1,0,3,2]
