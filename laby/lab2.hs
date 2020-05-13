triples :: Int -> [(Int,Int,Int)]
triples x = [(a,b,c)| a <- [1..x], b <- [1..x], c <- [1..x]]

triads :: Int -> [(Int,Int,Int)]
triads x = [(a,b,c) | (a, b, c) <- triples x, a^2 + b^2 == c^2]

nonTrivialTriads :: Int -> [(Int,Int,Int)]
nonTrivialTriads x = [(a,b,c) | (a, b, c) <- triples x, a^2 + b^2 == c^2,
                                a <= b, coprime a b]

coprime :: Int -> Int -> Bool
coprime a b = coprimeHelper a b (min a b)

coprimeHelper :: Int -> Int -> Int -> Bool
coprimeHelper a b 1 = True
coprimeHelper a b i = if a `mod` i == 0 && b `mod` i == 0
                        then False
                        else coprimeHelper a b (i-1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fibHelper 0 1 (n-2)

fibHelper :: Int -> Int -> Int -> Int
fibHelper x y 0 = x + y
fibHelper x y n = fibHelper y (x+y) (n-1)

myReverse :: [a] -> [a]
myReverse x = reverseHelper x []

reverseHelper :: [a] -> [a] -> [a]
reverseHelper [] rev = rev
reverseHelper [x] rev = x:rev
reverseHelper (x:xs) rev = reverseHelper xs (x:rev)

fact :: Int -> Int
fact n = factHelper n 1

factHelper :: Int -> Int -> Int
factHelper 0 acc = acc
factHelper x acc = factHelper (x-1) (x*acc)

indexOf :: Char -> String -> Maybe Int
indexOf c s = findIn c s 0

findIn :: Char -> String -> Int -> Maybe Int
findIn c [s] i = if c == s then Just i else Nothing
findIn c (s:ss) i = if c == s then Just i else findIn c ss (i+1)

positions :: Char -> String -> [Int]
positions c s = positionsHelper c s 0

positionsHelper :: Char -> String -> Int -> [Int]
positionsHelper c [s] i = if c == s then [i] else []
positionsHelper c (s:ss) i = if c == s
                            then i:positionsHelper c ss (i+1)
                            else positionsHelper c ss (i+1)

positions2 :: Char -> String -> [Int]
positions2 c s = [i | i <- [1..length s - 1], s!!i == c]

incAll :: [[Int]] -> [[Int]]
incAll [] = []
incAll [x] = [[y+1 | y <- x]]
incAll (x:xs) = incAll [x] ++ incAll xs

fact2 :: Int -> Int
fact2 n = foldr (*) 1 [1..n]

concat2 :: [[a]] -> [a]
concat2 (x:xs) = foldl (++) x xs

max2 :: [Int] -> Int
max2 (x:xs) = foldr max x xs

min2 :: [Int] -> Int
min2 (x:xs) = foldr min x xs

winner :: (a -> a -> a) -> [a] -> a
winner f (x:xs) = foldr f x xs

dotProduct :: [Int] -> [Int] -> Int
dotProduct x y = foldr (+) 0 (zipWith (*) x y)

nub :: Eq a => [a] -> [a]
nub [] = []
nub [a] = [a]
nub (x:xs) = x:(nub(filter (/=x) xs))