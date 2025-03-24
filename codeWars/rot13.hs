import Data.Char
import Data.List(elemIndex)


transform :: Int -> Char -> Char
transform 0 c = c
transform n c = case maybeIdx of
    Just idx -> cycle alphabet !! (idx + n)
    Nothing -> c
    where
        alphabet = if isLower c then ['a'..'z'] else ['A'..'Z']
        nLetters = length alphabet
        maybeIdx = c `elemIndex` alphabet



encrypt :: Int -> String -> String
encrypt _ "" = ""
encrypt n s = map f s
    where
        f = transform n

rot13 = encrypt 13