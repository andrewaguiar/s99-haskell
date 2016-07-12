
{- Insert an element at a given position into a list. -}
insertAt :: a -> Int -> [a] -> [a]
insertAt a 0 list = a:list
insertAt a x list | x > length list = list ++ [a]
                      | otherwise = l1 ++ [a] ++ l2
                      where (l1, l2) = splitAt x list

