
{- Create a list containing all integers within a given range. -}
{- Simplest solution could be 'range a y = [x..y] -}

range :: Int -> Int -> [Int]
range x y | x > y     = error "x cannot be lower than y"
          | x == y    = [x]
          | otherwise = [x] ++ range (x + 1) y
