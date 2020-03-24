module HashTree where
import Hashable32

data Tree a = Empty | Node Hash (Tree a) (Tree a) | Leaf Hash a -- czy kal mam leaf to potrzeba empty?

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

node :: Hashable a => Tree a -> Tree a -> Tree a
node l r = Node (combine (treeHash l) (treeHash r)) l r

-- na ilu poziomach może występować twig?
twig :: Hashable a => Tree a -> Tree a
twig l = Node (combine (treeHash l) (treeHash l)) l Empty

buildTree :: Hashable a => [a] -> Tree a
buildTree a = buildTreeLevel [] (convertToLeaves a) !! 0

convertToLeaves :: Hashable a => [a] -> [Tree a]
convertToLeaves [] = []
convertToLeaves (x:xs) = leaf x : convertToLeaves xs

buildTreeLevel :: Hashable a => [Tree a] -> [Tree a] -> [Tree a]
buildTreeLevel [] [a] = [a]
buildTreeLevel l [] = if length l <= 1 then l else buildTreeLevel [] l
buildTreeLevel l (a:[]) = buildTreeLevel [] (l ++ [twig a])
buildTreeLevel l (a:b:t) = buildTreeLevel (l ++ [node a b]) t

treeHash :: Hashable a => Tree a -> Hash
treeHash (Leaf x _) = x
treeHash (Node x _ _) = x

drawTree :: Show a => Tree a -> String
drawTree t = drawTreeHelper 1 t ++ "\n"

drawTreeHelper :: Show a => Int -> Tree a -> String
drawTreeHelper i (Leaf h a) = showHash h ++ " " ++ show a
drawTreeHelper i (Node a l Empty) = showHash a ++ " +\n" ++ replicate (2*i) ' ' ++ drawTreeHelper (i+1) l
drawTreeHelper i (Node a l r) = showHash a ++ " -\n"
                            ++ replicate (2*i) ' '
                            ++ drawTreeHelper (i+1) l
                            ++ "\n" ++ replicate (2*i) ' '
                            ++ drawTreeHelper (i+1) r
-- part B
type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

maybePrec = 0

-- to skopiowane ze stacka
newtype PlainString = PlainString String
instance Show PlainString where
  show (PlainString s) = s

instance Show a => Show (MerkleProof a) where
  showsPrec d (MerkleProof a []) = showParen (d > maybePrec) $ showsPrec (maybePrec+1) "Nothing"
  showsPrec d (MerkleProof a p) = showParen (d > maybePrec) $
                      showsPrec (maybePrec+1) (PlainString "MerkleProof ")
                      . showsPrec (11) a
                      . showString " "
                      . showString (showMerklePath [p])

showMerklePath :: [MerklePath] -> String
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
merklePaths e (Leaf h _)
    | hash e == h = [[]]
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

-- usunąć, moje testy
{- |
>>> putStr $ drawTree $ buildTree "fubar"
0x2e1cc0e4 -
  0xfbfe18ac -
    0x6600a107 -
      0x00000066 'f'
      0x00000075 'u'
    0x62009aa7 -
      0x00000062 'b'
      0x00000061 'a'
  0xd11bea20 +
    0x7200b3e8 +
      0x00000072 'r'

>>> print $ drawTree $ buildTree "a"
"0x00000061 'a'\n"

>>> buildProof 'e' $ buildTree "bitcoin"
Nothing

>>> let t = buildTree "bitcoin"
>>> let proof = buildProof 'i' t
>>> verifyProof (treeHash t) <$> proof
Just True

>>> verifyProof 0xbada55bb <$> proof
Just False

>>> merklePaths 'i' $ buildTree "bitcoin"
[[Left 1377068650,Left 1946203903,Right 98],[Right 1777612924,Left 1845538200,Right 111]]

>>> buildProof 'i' $ buildTree "bitcoin"
Just (MerkleProof 'i' <0x5214666a<0x7400b6ff>0x00000062)
-}


-- testy, które jeszcze nie chodzą
-- >>> mapM_ print $ map showMerklePath $ merklePaths 'i' $ buildTree "bitcoin"
-- "<0x5214666a<0x7400b6ff>0x00000062"
-- ">0x69f4387c<0x6e00ad98>0x0000006f"


