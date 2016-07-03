{- Flatten a nested list structure. -}
flattenList :: [[a]] -> [a]
flattenList [] = []
flattenList [x] = x
flattenList (h:t) = h ++ flattenList t
