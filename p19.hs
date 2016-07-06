
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

{- Rotate a list N places to the left. -}
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList 0 l = l
rotateList x l | x > 0 = lastPart x l ++ firstPart x l
               | otherwise = lastPart nx l ++ firstPart nx l
               where nx = length l - abs x
