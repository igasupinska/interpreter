import Control.Monad.Reader

-- task 1

-- allPairs :: [a] -> [a] -> [[a]]

-- task 2
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

renumber :: Tree a -> Tree Int
renumber t = renumberHelper t 0

renumberHelper :: Tree a -> Int -> Tree Int
renumberHelper Empty i = Empty
renumberHelper (Node a l r) i = Node i (renumberHelper l (i+1)) (renumberHelper r (i+1))

-- with Reader monad
renumber2 :: Tree a -> Tree Int
renumber2 t = runReader (renumber2Helper t) 0

-- renumber2Helper::  m      r   a
renumber2Helper :: Tree a -> Reader Int (Tree Int)
renumber2Helper (Node a l r) = do
    cur_lvl <- ask
    left <- local (+1) $ renumber2Helper l
    right <- local (+1) $ renumber2Helper r
    return (Node cur_lvl left right)
renumber2Helper Empty = return Empty