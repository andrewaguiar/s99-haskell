
extractSliceWithCounter :: Int -> Int -> Int -> [a] -> [a]
extractSliceWithCounter i1 i2 c [] = []
extractSliceWithCounter i1 i2 c (h:t) | between   = h:restList
                                      | otherwise = restList
                                      where between  = c >= i1 && c <= i2
                                            nc = c + 1
                                            restList = extractSliceWithCounter i1 i2 nc t

{- Extract a slice from a list. -}
extractSlice :: Int -> Int -> [a] -> [a]
extractSlice i1 i2 list = extractSliceWithCounter i1 i2 1 list

