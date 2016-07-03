my_head :: [a] -> a
my_head [] = error "cannot perform my_head in a empty list"
my_head (h:t) = h

{- Eliminate consecutive duplicates of list elements. -}
eliminateConsecutive :: (Eq a) => [a] -> [a]
eliminateConsecutive [] = []
eliminateConsecutive [x,y] | x == y = [y]
                           | otherwise = [x,y]
eliminateConsecutive (h:t) | h == my_head t = eliminateConsecutive t
                           | otherwise = h:eliminateConsecutive t
