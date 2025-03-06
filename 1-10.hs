e = []

o = [1]

t = [1, 2]

l = [1 .. 100]

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
