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

-- myPlus :: [a] -> [a] -> [a]
-- myPlus [] [] = []
-- myPlus [] x = x
-- myPlus x [] = x
-- myPlus x (y:ys) = myPlus (x:y) ys

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