
dropNthCount :: [a] -> Int -> Int -> [a]
dropNthCount [] _ _ = []
dropNthCount (h:t) x c | c == x = (dropNthCount t x 1)
                       | otherwise = h:(dropNthCount t x (c + 1))

{- Drop every Nth element from a list. -}
dropNth :: [a] -> Int -> [a]
dropNth l x = dropNthCount l x 1
