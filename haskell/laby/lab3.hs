foldMaybe :: c -> (a -> c) -> Maybe a -> c
foldMaybe x f Nothing = x
foldMaybe x f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe x (Just y) = y

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

-- maybeHead2 :: [a] -> Maybe a
-- maybeHead [] = Nothing
-- maybeHead2 a = Just (foldr )

foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither f g (Left a) = f a
foldEither f g (Right b) = g b

mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
mapEither f g (Left a) = Left (f a)
mapEither f g (Right b) = Right (g b)

mapRight ::  (b1 -> b2) -> Either a b1 -> Either a b2
mapRight f (Left a) = Left a
mapRight f (Right b) = Right (f b)

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

reverseRight :: Either e [a] -> Either e [a]
reverseRight (Left e) = Left e
reverseRight (Right a) = Right (reverse a)

-- trees

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Ord)

instance Show a => Show (Tree a) where
   show Empty = ""
   show (Node a Empty Empty) = show a
   show (Node a l r) = show l ++ " " ++ show a ++ " " ++ show r

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    Empty == Node a l r = False
    (Node a l r) == Empty = False
    (Node a l r) == (Node b m n) = (a == b) && (l == m) && (r == n)

toList :: Tree a -> [a]
toList Empty = []
toList (Node a Empty Empty) = [a]
toList (Node a l r) = toList l ++ a:(toList r)

-- bst tree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node a l r)
    | x == a = Node x l r
    | x < a  = Node a (insert x l) r
    | x > a  = Node a l (insert x r) 

contains :: (Ord a) => a -> Tree a -> Bool
contains x Empty = False
contains x (Node a l r)
    | x == a = True
    | x < a  = contains x l
    | x > a  = contains x r

fromList :: (Ord a) => [a] -> Tree a
fromList x = foldr insert Empty x

data Exp 
  = EInt Int             -- stała całkowita       
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2 

instance Eq Exp where
    (EInt x) == (EInt y) = x == y 

instance Show Exp where
  show (EInt x) = show x    
  show (EAdd e1 e2) = show e1 ++ " + " ++ show e2
  show (ESub e1 e2) = show e1  ++ " - " ++ show e2
  show (EMul e1 e2) = show e1 ++ " * " ++ show e2
  show (EVar x) = show x
  show (ELet x e1 e2) = "let " ++ show x ++ " = " ++ show e1 ++ " in " ++ show e2 

instance Num Exp where
    (+) x y = EAdd x y 
    (-) x y = ESub x y
    (*) x y = EMul x y
    negate x = ESub 0 x
    abs x = undefined
    signum x = undefined
    fromInteger x = EInt (fromIntegral x)

-- simplify Exp
-- simpl (EAdd (EMul (EInt 0) (EVar "x")) (EMul (EInt 1) (EVar "y")))
simpl :: Exp -> Exp
simpl (EInt x) = EInt x
simpl (EVar x) = EVar x
simpl (EMul x y) = simplOnce (EMul (simpl x) (simpl y))
simpl (EAdd x y) = simplOnce (EAdd (simpl x) (simpl y))

simplOnce :: Exp -> Exp
simplOnce (EMul (EInt 1) y) = simpl y
simplOnce (EMul y (EInt 1)) = simpl y
simplOnce (EMul (EInt 0) _) = EInt 0
simplOnce (EMul _ (EInt 0)) = EInt 0
simplOnce (EMul x y) = EMul x y
simplOnce (EAdd (EInt 0) x) = simpl x
simplOnce (EAdd x (EInt 0)) = simpl x
simplOnce (EAdd x y) = EAdd x y

testExp2 :: Exp
testExp2 = (2 + 2) * 3


