
{- Remove the K'th element from a list. -}
removeNth :: Int -> [a] -> (Maybe a, [a])
removeNth _ [] = (Nothing, [])
removeNth 0 (h:t) = (Just h, t)
removeNth x list | x > length list - 1 = error ("list doesnt have index " ++ show x)
                 | x < 0               = error "X cannot be negative"
                 | x > 0               = (Just h2, l1 ++ t2)
                 where (l1, (h2:t2)) = splitAt x list
