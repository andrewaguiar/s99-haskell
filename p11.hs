
data MyTuple a = One a | Two Int a

instance (Show a) => Show (MyTuple a) where
  show (One a) = show a
  show (Two x a) = "[" ++ (show x) ++ ":" ++ (show a) ++ "]"

trimInit :: [a] -> Int -> [a]
trimInit [] _ = []
trimInit l@(h:t) p | p == 0 = l
                   | otherwise = trimInit t (p - 1)

{- Modified run-length encoding. -}
runLengthEncoding :: (Eq a) => [a] -> [MyTuple a]
runLengthEncoding [] = []
runLengthEncoding [x] = [One x]
runLengthEncoding l@(h:t) | len > 1   = (Two len h):runLengthEncoding rest
                          | otherwise =     (One h):runLengthEncoding rest
                          where nList = takeWhile (\x -> x == h) l
                                len = length nList
                                rest = trimInit l len
