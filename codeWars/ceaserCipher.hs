module Rot13 where

import Data.Char
import Data.List (elemIndex)

addN :: Int -> Char -> Char
addN n c =
  case cIndex of
    Just ind -> letters !! ((ind + n) `mod` numLetters)
    Nothing  -> c
  where
    letters = ['A' .. 'Z'] ++ ['a' .. 'z']
    numLetters = length letters
    cIndex = c `elemIndex` letters

ceaserCipher :: Int -> String -> String
ceaserCipher _ "" = ""
ceaserCipher n s = map (addN n) s

rot13 = ceaserCipher 13