
{- Duplicate the elements of a list. -}
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (h:t) = [h,h] ++ duplicate t
