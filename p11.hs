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

{- Modified run-length encoding. -}
runLengthEncoding :: (Eq a) => [a] -> [a]
runLengthEncoding [] = []
runLengthEncoding [x] = [(x)]
runLengthEncoding l@(h:t) | sizeOf nList > 1 = ((sizeOf nList), h):runLengthEncoding (trimInit l (sizeOf nList))
                          | otherwise        =                 (h):runLengthEncoding (trimInit l (sizeOf nList))
                          where nList = takeWhile (\x -> x == h) l

