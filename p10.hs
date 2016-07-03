myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile fn [] = []
myTakeWhile fn (h:t) | fn(h) = h:myTakeWhile fn t
                     | otherwise = []

sizeOf :: [a] -> Int
sizeOf [] = 0
sizeOf [x] = 1
sizeOf (h:t) = 1 + (sizeOf t)

trimInit :: [a] -> Int -> [a]
trimInit [] _ = []
trimInit l@(h:t) p | p == 0 = l
                   | otherwise = trimInit t (p - 1)

{- Run-length encoding of a list. -}
runLengthEncoding :: (Eq a) => [a] -> [(Int, a)]
runLengthEncoding [] = []
runLengthEncoding [x] = [(1, x)]
runLengthEncoding l@(h:t) = ((sizeOf nList), h):runLengthEncoding (trimInit l (sizeOf nList))
                            where nList = takeWhile (\x -> x == h) l

