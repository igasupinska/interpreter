import Data.Char (isDigit)
import Data.Either
import Text.Read
import Control.Monad.Except
import Control.Monad.Error.Class


-- zadanie 1

fromRight :: b -> Either a b -> b
fromRight b (Left a) = b
fromRight b (Right a) = a

readInts2 :: String -> Either String [Int]
readInts2 s = readInts2Helper (words s)

readInts2Helper :: [String] -> Either String [Int]
readInts2Helper [] = Right []
readInts2Helper (s:ss) =
    case readEither s :: Either String Int of
        Left r -> Left $ "Not a number: " ++ s
        Right r -> if isRight rest
                    then Right (r:(fromRight [] rest))
                    else rest
        where
            rest = readInts2Helper ss

sumInts :: String -> String
sumInts s =
    case readInts2 s of
        Left s -> s
        Right s -> show $ foldr (+) 0 s


-- zadanie 2

data ParseError = Err {location::Int, reason::String}

type ParseMonad = Either ParseError

parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c i
    | c == '0' = Right 0
    | c == '1' = Right 1
    | c == '2' = Right 2
    | c == '3' = Right 3
    | c == '4' = Right 4
    | c == '5' = Right 5
    | c == '6' = Right 6
    | c == '7' = Right 7
    | c == '8' = Right 8
    | c == '9' = Right 9
    | c == 'a' = Right 10
    | c == 'b' = Right 11
    | c == 'c' = Right 12
    | c == 'd' = Right 13
    | c == 'e' = Right 14
    | c == 'f' = Right 15
    | otherwise = Left (Err i "Could not parse digit at index ") 


parseHex :: String -> ParseMonad Integer
parseHex s = parseHexHelper s 0

parseHexHelper :: String -> Int -> ParseMonad Integer
parseHexHelper [s] i = parseHexDigit s i
parseHexHelper (s:ss) i =
    case parseHexDigit s i of
        Left r -> Left r
        Right r -> if isRight rest
                    then Right (r*16 + (fromRight 0 rest))
                    else rest

        where
            rest = parseHexHelper ss (i+1)

toString :: Integer -> ParseMonad String
toString x = Right $ show x

-- convert zamienia napis z liczba szesnastkowa 
--   na napis z liczba dziesietna

convert :: String -> String
convert s = str
    where
        (Right str) = tryParse s `catchError` printError
        tryParse s = do {n <- parseHex s; toString n}
        printError e = Right $ reason e ++ show (location e)

-- printError: ParseError -> Either ParseError String

-- zadanie 4
mySequence :: Monad m => [m a] -> m [a]
mySequence [x] = do
    a <- x
    return [a]
mySequence (x:xs) = do
    a <- x
    as <- mySequence xs
    return (a:as)

myMapM :: Monad m => (a -> m b) -> [a] -> m [b]
myMapM f x = mySequence $ map f x 

myForM :: Monad m => [a] -> (a -> m b) -> m [b]
myForM l f = myMapM f l