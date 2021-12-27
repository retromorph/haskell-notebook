module Streams where
import qualified Prelude as P
import Data.Int (Int)

data Stream a = a :& Stream a
    deriving(P.Show)


head :: Stream a -> a
head (x:&xs) = x

tail :: Stream a -> Stream a
tail (x:&xs) = xs

(!!) :: Stream a -> P.Int -> a
x !! 0 = head x
(x:&xs) !! n = xs !! (n P.- 1)

take :: Stream a -> P.Int -> [a]
take x 0 = []
take (x:&xs) n = x : take xs (n P.- 1)

map :: (a -> b) -> Stream a -> Stream b
map f (x:&xs) = f x :& map f xs

filter :: (a -> P.Bool) -> Stream a -> Stream a
filter f (x:&xs)
    | f x = x :& rest
    | P.otherwise = rest
    where rest = filter f xs

zip :: Stream a -> Stream b -> Stream (a, b)
zip (a:&as) (b:&bs) = (a, b) :& zip as bs

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (a:&as) (b:&bs) = f a b :& zipWith f as bs

iterate :: (a -> a) -> a -> Stream a
iterate f a = f a :& iterate f (f a)