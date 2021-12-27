module BinTree where

data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)
    deriving(Eq, Show)

reverse :: BinTree a -> BinTree a
reverse (Leaf a) = Leaf a
reverse (Branch a b) = Branch b a

depth :: BinTree a -> Int
depth (Leaf a) = 1
depth (Branch a b) = 1 + max (depth a) (depth b)

leaves :: BinTree a -> [a]
leaves (Leaf a) = [a]
leaves (Branch a b) = leaves a ++ leaves b

