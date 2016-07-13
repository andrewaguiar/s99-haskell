import System.Random as Random

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (h:t) = t
removeAt x list | x > length list - 1 = error ("list doesnt have index " ++ show x)
                | x < 0               = error "X cannot be negative"
                | x > 0               = l1 ++ t2
                where (l1, (h2:t2)) = splitAt x list

{- Generate a random permutation of the elements of a list. -}

randomPermute :: [a] -> IO [a]
randomPermute [] = return []
randomPermute l = do
                    rindex <- Random.randomRIO(0, length l - 1)
                    rest <- randomPermute $ removeAt rindex l
                    return (l!!rindex:rest)

