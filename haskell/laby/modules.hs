import Data.Char (isDigit)

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

-- readInts2 :: String -> Either String [Int]
-- readInts2 s = all ()

