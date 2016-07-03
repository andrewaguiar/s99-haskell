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

{- Pack consecutive duplicates of list elements into sublists. -}
packConsecutives :: (Eq a) => [a] -> [[a]]
packConsecutives [] = []
packConsecutives [x] = [[x]]
packConsecutives l@(h:t) = [nList] ++ (packConsecutives (trimInit l (sizeOf nList)))
                           where nList = takeWhile (\x -> x == h) l

