module IsPrime where

isPrime :: Integer -> Bool
isPrime x 
    | x < 2 = False
    | otherwise = null divisors
  where divisors  = filter (\d -> x `mod` d == 0 ) $ candidates  x


candidates :: Integer -> [Integer]
candidates x
    | x < 2     = []
    | otherwise = [2 .. isqrt x]
  where
    isqrt :: Integer -> Integer
    isqrt = floor . sqrt . fromIntegral