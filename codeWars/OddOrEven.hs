module Kata.OddOrEven where

oddOrEven :: (Integral a) => [a] -> String
oddOrEven xs = if even $ sum xs then "even" else "odd"