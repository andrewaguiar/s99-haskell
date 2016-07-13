
import System.Random as Random

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (h:t) = t
removeAt x list | x > length list - 1 = error ("list doesnt have index " ++ show x)
                | x < 0               = error "X cannot be negative"
                | x > 0               = l1 ++ t2
                where (l1, (h2:t2)) = splitAt x list

{- Extract a given number of randomly selected elements from a list. -}

extractRandom :: [a] -> Int -> IO [a]
extractRandom [] _ = return []
extractRandom l 0 = return []
extractRandom l x = do
                      rindex <- Random.randomRIO(0, length l - 1)
                      rest <- extractRandom (removeAt rindex l) (x - 1)
                      return (l!!rindex:rest)
