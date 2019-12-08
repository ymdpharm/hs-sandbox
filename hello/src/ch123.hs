doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else doubleMe x

charName :: Char -> String
charName 'a' = "this is A"
charName 'b' = "this is B"

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

head' :: [a] -> a
head' [] = error "empty."
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of [] -> error "empty."
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is "
                  ++ case xs of [] -> "empty."
                                [x] -> "a singleton list."
                                xs -> "a longer list."

