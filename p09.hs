trimInit :: [a] -> Int -> [a]
trimInit [] _ = []
trimInit l@(h:t) p | p == 0 = l
                   | otherwise = trimInit t (p - 1)

{- Pack consecutive duplicates of list elements into sublists. -}
packConsecutives :: (Eq a) => [a] -> [[a]]
packConsecutives [] = []
packConsecutives [x] = [[x]]
packConsecutives l@(h:t) = [nList] ++ (packConsecutives rest)
                           where nList = takeWhile (\x -> x == h) l
                                 rest = trimInit l (length nList)

