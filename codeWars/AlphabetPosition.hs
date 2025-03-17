module AlphabetPosition (alphabetPosition) where

import Data.Char (isAscii, isLetter, ord, toLower)

alphabetPosition :: String -> String
alphabetPosition str = unwords $ map (show . (\x -> ord x - magic)) sanitized
  where
    sanitized = [toLower s | s <- str, isAscii s, isLetter s]
    magic = 96