l = [1..]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


sq :: Num a => a -> a
sq x = x * x

mapped = map' sq (take 10 l)


foldLeft' :: (a -> a -> a) -> a -> [a] -> a
foldLeft' _ acc [] = acc
foldLeft' f acc (x:xs) = foldLeft' f  (f acc x) xs

add' :: Num a => a -> a -> a
add' left right = left + right

sum' :: Num a => [a] -> a
sum' = foldLeft' (+) 0

reduced = foldLeft' add' 0 (take 10 l)


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f li = [x | x <- li, f(x) ]

evens = filter' even (take 100 l)