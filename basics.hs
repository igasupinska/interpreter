doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

-- doubleSmallNumber x = if x > 100
--                      then 100
--                      else 2*x

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list has no maximum!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' 0 x = []
replicate' n x = x:(replicate' (n-1) x)

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' 0 x = []
take' n [] = []
take' n (x:xs) = x:(take' (n-1) xs)

reverse' :: [a] -> [a]  
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]  
zip' [] x = []
zip' x [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool  
elem' x [] = False
elem' x (y:ys) = x == y || elem' x ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [z | z <- xs, z < x]
                   ++ [x]
                   ++ quicksort [z | z <- xs, z > x]

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a l = foldl (\x y -> if y == a then True else x) False l