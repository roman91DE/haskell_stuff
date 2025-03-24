module Pangram where
import Data.Char (toLower, isLetter)



isPangram :: String -> Bool
isPangram s
    | length str < 26 = False
    | otherwise = all (`elem` str) letters
    where
        letters = ['a'..'z']
        clean = filter isLetter . map toLower
        str = clean s