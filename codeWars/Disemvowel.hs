module Disemvowel (disemvowel) where

disemvowel :: String -> String
disemvowel str = [s | s <- str, s `notElem` vowels]
  where
    vowels = "aeiouAEIOU"