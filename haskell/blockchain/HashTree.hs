import Hashable32

data Tree a = Empty | Node Hash (Tree a) (Tree a)

leaf :: Hashable a => a -> Tree a
leaf a = Node (hash a) Empty Empty

node :: Hashable a => Tree a -> Tree a -> Tree a
node l r = Node (combine (treeHash l) (treeHash r)) l r

twig :: Hashable a => Tree a -> Tree a
twig l = Node (combine (treeHash l) (treeHash l)) l Empty

buildTree :: Hashable a => [a] -> Tree a
buildTree a = buildTreeLevel [] (convertToLeaves a) !! 0

convertToLeaves :: Hashable a => [a] -> [Tree a]
convertToLeaves [] = []
convertToLeaves [a] = [leaf a]
convertToLeaves (x:xs) = leaf x : convertToLeaves xs

buildTreeLevel :: Hashable a => [Tree a] -> [Tree a] -> [Tree a]
buildTreeLevel l [] = if length l <= 1 then l else buildTreeLevel [] l
buildTreeLevel l (a:[]) = l ++ [twig a]
buildTreeLevel l (a:b:t) = buildTreeLevel (l ++ [node a b]) t

treeHash :: Hashable a => Tree a -> Hash
treeHash (Node x _ _) = x

drawTree :: Show a => Tree a -> String
drawTree (Node a Empty Empty) = showHash a ++ "\n"
drawTree (Node a l Empty) = showHash a ++ "+\n  " ++ drawTree l
drawTree (Node a l r) = showHash a ++ "-\n  " ++ drawTree l ++ "\n  " ++ drawTree r