triples :: Int -> [(Int,Int,Int)]
triples x = [(a,b,c)| a <- [1..x], b <- [1..x], c <- [1..x]]

triads :: Int -> [(Int,Int,Int)]
triads x = [(a,b,c) | (a, b, c) <- triples x, a^2 + b^2 == c^2]

-- nonTrivialTriads :: Int -> [(Int, Int, Int)]
-- nonTrivialTriads

removePermutationsFromTriads :: [(Int, Int, Int)] -> [(Int, Int, Int)]
removePermutationsFromTriads [] = []
removePermutationsFromTriads [x] = [x]
removePermutationsFromTriads (x:xs) = let (a, b, c) = x
                                    in 
                                    (if (b, a, c) `elem` xs
                                    then removePermutationsFromTriads xs
                                    else x:removePermutationsFromTriads xs)

-- removeMultiplicationsFromTriads :: [(Int, Int, Int)] -> [(Int, Int, Int)]
-- removeMultiplicationsFromTriads [] = []
-- removeMultiplicationsFromTriads [x] = [x]
-- removeMultiplicationsFromTriads x:xs = let (a, b, c) = x
--                                         in
--                                         (if )

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