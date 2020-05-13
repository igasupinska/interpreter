import Text.ParserCombinators.Parsec
import Data.Char(isDigit,digitToInt)

run :: Parser a -> [Char] -> Either ParseError a
run p s = parse p "(interactive)" s

pDigit :: Parser Int
pDigit = satisfy isDigit >>= return . digitToInt
-- pDigit = fmap digitToInt digit

pDigits :: Parser [Int]
pDigits = many1 pDigit

pNat :: Parser Integer
pNat = fmap (foldl (\x y -> 10*x+(toInteger y)) 0) pDigits

pInt :: Parser Integer
pInt = negative pNat <|> pNat where
  negative :: (Num a) => Parser a -> Parser a
  negative p = fmap negate (char '-' >> p)