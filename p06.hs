
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h:t) = reverseList t ++ [h]

{- Find out whether a list is a palindrome. -}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = l == (reverseList l)

