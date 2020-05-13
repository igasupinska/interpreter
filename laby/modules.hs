import Data.Char (isDigit)
import Data.Either

readInts :: String -> [Int]
readInts s = [x | Just x <- map readInt (words s)]

readInt :: String -> Maybe Int
readInt s = if all isDigit s
                then Just (turnToInt s 0)
                else Nothing

turnToInt :: String -> Int -> Int
turnToInt "1" i = i*10 + 1
turnToInt "2" i = i*10 + 2
turnToInt "3" i = i*10 + 3
turnToInt "4" i = i*10 + 4
turnToInt "5" i = i*10 + 5
turnToInt "6" i = i*10 + 6
turnToInt "7" i = i*10 + 7
turnToInt "8" i = i*10 + 8
turnToInt "9" i = i*10 + 9
turnToInt "0" i = i*10
turnToInt [s] i = i*10 + turnToInt [s] i
turnToInt (s:ss) i = turnToInt ss (turnToInt [s] i) 

mapRight ::  (b1 -> b2) -> Either a b1 -> Either a b2
mapRight f (Left a) = Left a
mapRight f (Right b) = Right (f b)

reverseRight :: Either e [a] -> Either e [a]
reverseRight (Left e) = Left e
reverseRight (Right a) = Right (reverse a)


fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

fromRight :: b -> Either a b -> b
fromRight b (Left a) = b
fromRight b (Right a) = a

readInts2 :: String -> Either String [Int]
readInts2 s = readInts2Helper (words s)

readInts2Helper :: [String] -> Either String [Int]
readInts2Helper [] = Right []
readInts2Helper (s:ss) = let x = readInt2 s in
                            if isLeft x
                                then x
                                else
                                    let rest = readInts2Helper ss in
                                     if isRight rest
                                         then Right (fromRight [] x ++ fromRight [] rest)
                                         else rest

readInt2 :: String -> Either String [Int]
readInt2 s = if all isDigit s
                then Right [turnToInt s 0]
                else Left ("Not a number " ++ s)

sumInts :: String -> String
sumInts s = sumIntsHelper (readInts2 s)

sumIntsHelper :: Either String [Int] -> String
sumIntsHelper (Left s) = s
sumIntsHelper (Right s) = show (foldr (+) 0 s)