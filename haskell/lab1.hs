mul x y = x * y
square x = mul x x
area r = pi * square r


fact 0 = 1
fact n = n * fact (n-1)

len [] = 0
len (h:t) = 1 + len t

splitBy :: Int -> [Int] -> ([Int],[Int])
splitBy n [] = ([],[])
splitBy n (x:xs) = let (ys,zs) = splitBy n xs in
    if x<= n then (x:ys,zs) else (ys,x:zs)

splitBy' n (x:xs)
    | x<=n = let (ys,zs)=splitBy' n xs in (x:ys,zs)
    | x>n = let (ys,zs)=splitBy' n xs in (ys,x:zs)

myHead :: [a]  -> a
myHead (x:xs) = x

myTail :: [a]  -> a
myTail [x] = x
myTail (x:xs) = myTail xs

myPlus :: [a] -> [a] -> [a]
myPlus [] [] = []
myPlus [] x = x
myPlus x [] = x
myPlus [x] y = x:y
myPlus (x:xs) y = x:(myPlus xs y)

myTake :: [a] -> Int -> [a]
myTake [] i = []
myTake x 0 = []
myTake (x:xs) i = (x:myTake xs (i-1))

myDrop :: [a] -> Int -> [a]
myDrop [] i = []
myDrop x 0 = x
myDrop (x:xs) i = myDrop xs (i-1)

myFilter :: (a->Bool) -> [a] -> [a]
myFilter cond [] = []
myFilter cond (x:xs) = if cond x then (x:myFilter cond xs) else myFilter cond xs

myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits [x] = [[x]] ++ myInits []
myInits x = [x] ++ myInits (myTake x (len x - 1))

inits :: [a] -> [[a]]
inits a = [myTake a i | i <- [0..len a]]

inits2 :: [a] -> [[a]]
inits2 [] = [[]]
inits2 [x] = [[], [x]]
inits2 (x:xs) = let y = (inits xs)
                in ([[]] ++ [x:z | z <- y])

partitions :: [a] -> [([a], [a])]
partitions a = myPartitions a (len a)

myPartitions :: [a] -> Int -> [([a], [a])]
myPartitions [] x = [([],[])]
myPartitions x 0 = []
myPartitions [x] 1 = [([x], []), ([], [x])]
myPartitions x y = [(myTake x y, myDrop x y)] ++ myPartitions x (y-1)

nub :: Eq a => [a] -> [a]
nub [] = []
nub [a] = [a]
nub (x:xs) = let y = (nub xs)
    in (if (elem x y) then y else [x] ++ y)

-- permutations :: [a] -> [[a]]
-- permutations [] = [[]]
-- permutations [x] = [[x]]
-- permutations x = let y = partitions x in
--                 permutationsHelper [z | z <- y]

-- -- permutacje dla jednej pary odcinków
-- permutationsHelper :: ([a], [a]) -> [[a]]
-- permutationsHelper ([x], [y]) = [[x:y], [y:x]]
-- permutationsHelper ([x], y) = let z = permutations y in
--                             ([x:v | v <- z] ++ [v:x | v <-z])