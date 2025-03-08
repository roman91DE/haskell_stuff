collatz :: (Integral a) => a -> a
collatz n
    | mod n 2 == 0 = n `div` 2
    | otherwise = (3 * n) + 1



conj :: (Integral a) => a -> [a]

