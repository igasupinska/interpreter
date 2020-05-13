import Text.ParserCombinators.Parsec
import Data.Char(isDigit,digitToInt)
import Text.Parsec


data Exp 
  = EInt Int             -- stała całkowita       
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2


run :: Parser a -> [Char] -> Either ParseError a
run p s = parse p "(interactive)" s

pExp :: Parser Exp

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