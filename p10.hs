trimInit :: [a] -> Int -> [a]
trimInit [] _ = []
trimInit l@(h:t) p | p == 0 = l
                   | otherwise = trimInit t (p - 1)

{- Run-length encoding of a list. -}
runLengthEncoding :: (Eq a) => [a] -> [(Int, a)]
runLengthEncoding [] = []
runLengthEncoding [x] = [(1, x)]
runLengthEncoding l@(h:t) = (len, h):runLengthEncoding rest
                            where nList = takeWhile (\x -> x == h) l
                                  len = length nList
                                  rest = trimInit l len

