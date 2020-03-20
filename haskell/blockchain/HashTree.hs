module HashTree where
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
buildTreeLevel [] [a] = [a]
buildTreeLevel l [] = if length l <= 1 then l else buildTreeLevel [] l
buildTreeLevel l (a:[]) = buildTreeLevel [] (l ++ [twig a])
buildTreeLevel l (a:b:t) = buildTreeLevel (l ++ [node a b]) t

treeHash :: Hashable a => Tree a -> Hash
treeHash (Node x _ _) = x

drawTree :: Show a => Tree a -> String
drawTree t = drawTreeHelper 1 t ++ "\n"

drawTreeHelper :: Show a => Int -> Tree a -> String
drawTreeHelper i (Node a Empty Empty) = showHash a ++ " ?" -- dopisać znaki
drawTreeHelper i (Node a l Empty) = showHash a ++ " +\n" ++ replicate (2*i) ' ' ++ drawTreeHelper (i+1) l
drawTreeHelper i (Node a l r) = showHash a ++ " -\n"
                            ++ replicate (2*i) ' '
                            ++ drawTreeHelper (i+1) l
                            ++ "\n" ++ replicate (2*i) ' '
                            ++ drawTreeHelper (i+1) r
-- part B
type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
    show (MerkleProof a []) = "Nothing"
    show (MerkleProof a p) = "MerkleProof " ++ show a ++ " " ++ showMerklePath [p]

showMerklePath :: [MerklePath] -> String
showMerklePath [] = ""
showMerklePath [p] = showMerklePathHelper p
showMerklePath (p:ps) = showMerklePathHelper p ++ "\n" ++ showMerklePath ps

showMerklePathHelper :: MerklePath -> String
showMerklePathHelper [] = ""
showMerklePathHelper [Left a] = "<" ++ showHash a
showMerklePathHelper [Right a] = ">" ++ showHash a
showMerklePathHelper (p:ps) = showMerklePathHelper [p] ++ showMerklePathHelper ps

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof e Empty = Nothing
buildProof e t = let p = merklePaths e t in
                    if length p /= 0
                        then Just (MerkleProof e (p!!0))
                        else Nothing


merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths e Empty = []
merklePaths e (Node a Empty Empty)
    | hash e == a = [[]]
    | otherwise = []
merklePaths e (Node a l Empty)
    | hash e == treeHash l = [[Left $ treeHash l]] -- twig has duplicated child
    | otherwise = []
merklePaths e (Node a l r)
    | hash e == treeHash l = [[Left $ treeHash r]]
    | hash e == treeHash r = [[Right $ treeHash l]]
    | otherwise = let pl = merklePaths e l
                      pr = merklePaths e r
                  in
                    if length pl == 0 && length pr == 0
                    then []
                    else
                        if length pl /= 0 && length pr /= 0
                        then map (Left (treeHash r) :) pl ++ map (Right (treeHash l):) pr
                        else
                            if length pr /= 0
                            then map (Right (treeHash l):) pr
                            else map (Left (treeHash r):) pl


verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a []) = False
verifyProof h p = h == verifyProofHelper p

-- sprawdzić kierunki, bo jakoś dziwnie
verifyProofHelper :: Hashable a => MerkleProof a -> Hash
verifyProofHelper (MerkleProof a [Left p]) = combine (hash a) p
verifyProofHelper (MerkleProof a [Right p]) = combine p (hash a)
verifyProofHelper (MerkleProof a (Left p:ps)) = combine (verifyProofHelper (MerkleProof a ps)) p
verifyProofHelper (MerkleProof a (Right p:ps)) = combine p (verifyProofHelper (MerkleProof a ps))

