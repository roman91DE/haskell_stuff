{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use product" #-}

l = [1..100]

duplicate' :: [a] -> [a]
duplicate' = foldr (\x -> (++) [x, x]) []

sum' :: (Num a) => [a] -> a
sum' = foldr (+) 0

prod' :: (Num a) => [a] -> a
prod' = foldr (*) 1

len' :: [a] -> Integer
len' = foldr (\_ -> (+) 1) 0

reverse' :: [a] ->[a]
reverse' = foldl (\acc x -> x : acc) []

contains' :: Eq a => a -> [a] -> Bool
contains' val = foldr (\x acc -> (x == val) || acc) False

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x -> (:) (f x)  ) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x -> if p x then (:) x else id) []

replElems :: Int -> [a] -> [a]
replElems n = foldr (\x -> (++) (replicate n x)) []

splitAt' :: Int -> [a] -> ([a],[a])
-- splitAt' n xs = (take n xs, drop n xs ) 
splitAt' n xs = (reverse first, reverse second)
    where
        (first, second) = foldl (\(f,s) x -> if length f < n then (x:f, s) else (f, x:s)) ([], []) xs