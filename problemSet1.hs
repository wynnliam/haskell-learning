lastElem :: [a] -> a
lastElem [x] = x
lastElem [] = error "No last item in empty list!"
lastElem (x : xs) = lastElem xs

secondToLast :: [a] -> a
secondToLast [x, y] = x
secondToLast [x] = error "Only one item!"
secondToLast [] = error "No items in list!"
secondToLast (x : xs) = secondToLast xs

elemAt :: [a] -> Int -> a
elemAt l x = l !! (x - 1)

listSize :: [a] -> Int
listSize [] = 0
listSize (x : xs) = 1 + (listSize xs)

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x : xs) = (reverse1 xs) ++ [x]

palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome xs = (head xs) == (last xs) &&
                (palindrome $ init $ tail xs)

main = do
  putStrLn (show (palindrome [1, 2, 1]))
