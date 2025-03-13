-- https://wiki.haskell.org/index.php?title=99_questions/1_to_10

-- test lists
e = []

o = [1]

t = [1, 2]

l = [1 .. 100]

isPali = "anna"
notPali = "klaus"



last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = Just (last xs)

secLast :: [a] -> Maybe a
secLast [] = Nothing
secLast [x] = Nothing
secLast xs = Just (head (tail (reverse xs)))

kthElem :: (Integral k) => k -> [a] -> Maybe a
kthElem _ [] = Nothing
kthElem k (x : xs)
  | k < 1 = Nothing
  | k == 1 = Just x
  | otherwise = kthElem (k - 1) xs

myReverse :: [a] -> [a]
myReverse xs = myReverse' [] xs
  where
    myReverse' :: [a] -> [a] -> [a]
    myReverse' acc [] = acc
    myReverse' acc (x : xs) = myReverse' (x : acc) xs

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [a] = True
isPalindrom xs =
  head xs == last xs && isPalindrom (init (tail xs))



flatten :: [[a]] -> [a]
flatten xs = flattenAcc [] xs
  where
    flattenAcc acc [] = acc
    flattenAcc acc (x:xs) = flattenAcc (acc ++ x) xs


