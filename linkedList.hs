module LinkedList where

data LinkedList a = Empty | Node (a, LinkedList a)

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
