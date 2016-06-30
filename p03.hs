
{- Find the Kth element of a list. -}
nth :: [a] -> Int -> a
nth [] _ = error "cannot perform nth in a empty list"
nth (h:t) 0 = h
nth (h:t) p = nth t (p - 1)
