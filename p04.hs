
{- Find the number of elements of a list. -}
sizeOfList :: [a] -> Int
sizeOfList [] = 0
sizeOfList (h:t) = sizeOfList t + 1
