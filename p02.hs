
{- Find the last but one element of a list. -}
penultimate :: [a] -> a
penultimate [] = error "cannot perform penultimate in a empty list"
penultimate [_] = error "cannot perform penultimate in a 1 element list"
penultimate [x,_] = x
penultimate (head:tail) = penultimate tail
