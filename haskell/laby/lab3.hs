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