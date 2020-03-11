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
