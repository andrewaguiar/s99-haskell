
lastPart :: Int -> [a] -> [a]
lastPart _ [] = []
lastPart _ [x] = [x]
lastPart x l@(h:t) | x > 0     = lastPart nx t
                   | otherwise = l
                   where nx = x - 1

firstPart :: Int -> [a] -> [a]
firstPart _ [] = []
firstPart _ [x] = [x]
firstPart x l@(h:t) | x > 0 = h:(firstPart nx t)
                    | otherwise = []
                    where nx = x - 1


{- Split a list into two parts. -}
splitListTwoParts :: Int -> [a] -> [[a]]
splitListTwoParts 0 l = [l]
splitListTwoParts _ [] = [[]]
splitListTwoParts _ [x] = [[x]]
splitListTwoParts x l = [firstPart x l, lastPart x l]
