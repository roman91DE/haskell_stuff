module LinkedList where

data LinkedList a = Empty | Node (a, LinkedList a) deriving (Eq)

instance (Show a) => Show (LinkedList a) where
  show Empty = "Empty"
  show (Node (x, xs)) = show x ++ " -> " ++ show xs

instance Functor LinkedList where
  fmap = map'

prepend :: a -> LinkedList a -> LinkedList a
prepend elem list = Node (elem, list)

append :: a -> LinkedList a -> LinkedList a
append elem Empty = prepend elem Empty
append elem (Node (v, next)) = Node (v, nextNode)
  where
    nextNode = append elem next

filter' :: (a -> Bool) -> LinkedList a -> LinkedList a
filter' _ Empty = Empty
filter' p (Node (v, next)) = if p v then Node (v, filter' p next) else filter' p next

map' :: (a -> b) -> LinkedList a -> LinkedList b
map' _ Empty = Empty
map' f (Node (v, next)) = Node (f v, map' f next)

reduce' :: (b -> a -> b) -> b -> LinkedList a -> b
reduce' _ acc Empty = acc
reduce' f acc (Node (val, next)) = reduce' f newAcc next
  where
    newAcc = f acc val

length' :: LinkedList a -> Int
length' = reduce' (\acc _ -> acc + 1) 0

sum' :: (Num a) => LinkedList a -> a
sum' = reduce' (+) 0

prod' :: (Num a) => LinkedList a -> a
prod' = reduce' (*) 1

indexOf :: (Eq a) => LinkedList a -> a -> Maybe Int
indexOf xs elem = f xs elem 0
  where
    f Empty _ _ = Nothing
    f (Node (v, next)) elem currentIdx
      | v == elem = Just currentIdx
      | otherwise = f next elem (currentIdx + 1)

contains' :: (Eq a) => LinkedList a -> a -> Bool
contains' xs elem = case indexOf xs elem of
  Just _ -> True
  Nothing -> False

concat' :: LinkedList a -> LinkedList a -> LinkedList a
concat' Empty ys = ys
concat' (Node (v, next)) ys = Node (v, concat' next ys)

take' :: Int -> LinkedList a -> LinkedList a
take' _ Empty = Empty
take' n _ | n <= 0 = Empty
take' n (Node (v, next)) = Node (v, take' (n - 1) next)



-- tl = Empty

-- t1 = prepend 1 tl

-- t2 = prepend 2 t1

-- t3 = prepend 3 t2

-- t4 = prepend 4 t3
