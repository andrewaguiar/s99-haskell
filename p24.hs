
import System.Random as Random

removeAt :: Int -> [Int] -> [Int]
removeAt _ [] = []
removeAt 0 (h:t) = t
removeAt x list | x > length list - 1 = error ("list doesnt have index " ++ show x)
                | x < 0               = error "X cannot be negative"
                | x > 0               = l1 ++ t2
                where (l1, (h2:t2)) = splitAt x list

range :: Int -> Int -> [Int]
range x y | x > y     = error "x cannot be lower than y"
          | x == y    = [x]
          | otherwise = [x] ++ range (x + 1) y


extractRandom :: [Int] -> Int -> IO [Int]
extractRandom [] _ = return []
extractRandom l 0 = return []
extractRandom l x = do
                      rindex <- Random.randomRIO(0, length l - 1)
                      rest <- extractRandom (removeAt rindex l) (x - 1)
                      return (l!!rindex:rest)

{- Lotto: Draw N different random numbers from the set 1..M. -}

drawXRandoms :: Int -> Int -> IO [Int]
drawXRandoms n x = extractRandom (range 1 x) n
