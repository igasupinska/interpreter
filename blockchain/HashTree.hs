module HashTree where
import Hashable32
import Utils

data Tree a = Empty | Node Hash (Tree a) (Tree a) | Leaf Hash a

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

node :: Hashable a => Tree a -> Tree a -> Tree a
node l r = Node (combine (treeHash l) (treeHash r)) l r

twig :: Hashable a => Tree a -> Tree a
twig l = Node (combine (treeHash l) (treeHash l)) l Empty

buildTree :: Hashable a => [a] -> Tree a
buildTree l = buildTreeLevel [] (map leaf l) !! 0

buildTreeLevel :: Hashable a => [Tree a] -> [Tree a] -> [Tree a]
buildTreeLevel [] [a] = [a]
buildTreeLevel l [] = buildTreeLevel [] l
buildTreeLevel l [a] = buildTreeLevel [] (l ++ [twig a])
buildTreeLevel l (a:b:t) = buildTreeLevel (l ++ [node a b]) t

treeHash :: Hashable a => Tree a -> Hash
treeHash (Leaf x _) = x
treeHash (Node x _ _) = x

drawTree :: Show a => Tree a -> String
drawTree Empty = []
drawTree t = drawTreeLevel 0 t ++ "\n"

drawTreeLevel :: Show a => Int -> Tree a -> String
drawTreeLevel lvl (Leaf h a) = indentNewLine (2*lvl) ++ showHash h ++ " " ++ show a
drawTreeLevel lvl (Node a l Empty) = indentNewLine (2*lvl)
                                    ++ showHash a
                                    ++ " +"
                                    ++ drawTreeLevel (lvl+1) l
drawTreeLevel lvl (Node a l r) = indentNewLine(2*lvl)
                                ++ showHash a
                                ++ " -"
                                ++ drawTreeLevel (lvl+1) l
                                ++ drawTreeLevel (lvl+1) r

indentNewLine :: Int -> String
indentNewLine 0 = []
indentNewLine n = "\n" ++ replicate n ' '

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
  showsPrec d (MerkleProof a []) = showString "Nothing"
  showsPrec d (MerkleProof a p) = showParen (d > 0) $
                        showString "MerkleProof "
                      . showsPrec (maxPrec) a
                      . showString " "
                      . showString (showMerklePath p)
                  where maxPrec = 11

showMerklePath :: MerklePath -> String
showMerklePath [] = []
showMerklePath [Left a] = "<" ++ showHash a
showMerklePath [Right a] = ">" ++ showHash a
showMerklePath (p:ps) = showMerklePath [p] ++ showMerklePath ps

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof e t = do
  path <- maybeHead (merklePaths e t)
  return $ MerkleProof e path

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths e Empty = []
merklePaths e (Leaf h _)
    | hash e == h = [[]]
    | otherwise = []
merklePaths e (Node a l Empty) = fmap (Left (treeHash l):) (merklePaths e l)
merklePaths e (Node a l r) = fmap (Left (treeHash r):) (merklePaths e l)
                          ++ fmap (Right (treeHash l):) (merklePaths e r)

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a []) = False
verifyProof h p = h == verifyProofHelper p

verifyProofHelper :: Hashable a => MerkleProof a -> Hash
verifyProofHelper (MerkleProof a [Left p]) = combine (hash a) p
verifyProofHelper (MerkleProof a [Right p]) = combine p (hash a)
verifyProofHelper (MerkleProof a (Left p:ps)) = combine (verifyProofHelper (MerkleProof a ps)) p
verifyProofHelper (MerkleProof a (Right p:ps)) = combine p (verifyProofHelper (MerkleProof a ps))
