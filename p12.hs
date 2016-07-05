
{- Decode a run-length encoded list. -}
decode :: [(Int, a)] -> [a]
decode [] = []
decode [(0, a)] = []
decode [(1, a)] = [a]
decode [(x, a)] = [a] ++ decode [(x - 1, a)]
decode (h:t) = (decode [h]) ++ (decode t)
