module Codewars.Kata.Categorize where

data Membership = Open | Senior deriving (Eq, Show)

openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior = map classify
  where
    classify :: (Int, Int) -> Membership
    classify (age, handicap) = if age >= 55 && handicap > 7 then Senior else Open