applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- filp' :: (a -> b -> c) -> (b -> a -> c)
-- filp' f = g
--     where g x y = f y x

filp' :: (a -> b -> c) -> b -> a -> c
filp' f y x = f x y

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n * 3 + 1)

--numLongChains :: Int
--numLongChains = length (filter isLong (map chain [1..100]))
--    where isLong xs = length xs > 15

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

