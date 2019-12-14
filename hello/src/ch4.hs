module Ch4 where

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list."
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x 
    | n <= 0 = []
    | otherwise = x : replicate' (n - 1) x 

take' :: Int -> [a] -> [a]
take' n xs
    | n <= 0 = []
take' _ [] = []
take' n (x: xs) = x : take (n - 1) xs

reverse' :: [a] -> [a]
reverse' [x] = [x]
reverse' (x: xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x , y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' e [] = False
elem' e (x : xs) 
    | e == x = True
    | otherwise = elem' e xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = 
    let smaller = [a | a <- xs, a <= x]
        bigger = [a | a <- xs, a > x]
    in quickSort smaller ++ [x] ++ quickSort bigger

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x: xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x: xs) 
    | f x = x : filter' f xs
    | otherwise = filter' f xs