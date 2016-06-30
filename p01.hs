
{- Find the last element of a list. -}
last' :: [a] -> a
last' [] = error "cannot perform last in a empty list"
last' [x] = x
last' (head:tail) = last tail
