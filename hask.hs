--2. Определите функци, функцию, заменяющую в исходном списке все вхождения заданного значения другим.

m lst from to = map 
    (\x -> if x == from
    then to else x) lst

main = do
print $ m [1,2,3,2,3] 2 5


-- 13. Реализовать на языке Haskell функцию, которая, чередуя элементы списков [a, b, ...] и [1, 2, ...], образует 

новыи? список [a, 1, b, 2, ...].
m [] x = if x == [] then x else m x []
m (x:xs) y = x : mergelst y xs

main = do
print $ m ["1", "2", "3"] ["a", "b", "c"]
print $ m ["1", "2", "3"] ["a"]


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
