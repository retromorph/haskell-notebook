module Fix where
import Data.List
import Data.Function

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs -- Y (f, (x:xs)) = f(x) ++ Y (f, xs)

-- map'' :: (a -> b) -> [a] -> [b]
-- map'' f a = 

-- foldr' :: (a -> b -> b) -> b -> [a] -> b
-- foldr' f a as = fix $ \xs -> f a xs

-- zip' :: [a] -> [b] -> [(a, b)]
-- zip' as bs = fix . (,)

repeat' :: a -> [a]
repeat' = fix . (:)

cycle' :: [a] -> [a]
cycle' = fix . (++)

iterate' :: (a -> a) -> a -> [a]
iterate' f = fix . (:) . f