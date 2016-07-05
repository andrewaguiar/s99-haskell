
{- Duplicate the elements of a list a given number of times. -}
duplicateX :: [a] -> Int -> [a]
duplicateX [] _ = []
duplicateX _ 0 = []
duplicateX l 1 = l
duplicateX (h:t) x = [h | _ <- [1..x]] ++ (duplicateX t x)
