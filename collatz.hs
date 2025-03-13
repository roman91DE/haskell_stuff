collatz :: (Integral a) => a -> a
collatz n
  | even n = n `div` 2
  | otherwise = (3 * n) + 1

collatzSequence :: (Integral a) => a -> [a]
collatzSequence n = cj [] n
  where
    cj :: (Integral a) => [a] -> a -> [a]
    cj acc n
      | n == 1 = reverse (n : acc)
      | otherwise = cj (n : acc) (collatz n)
