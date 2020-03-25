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
drawTree t = drawTreeHelper 0 t ++ "\n"

drawTreeHelper :: Show a => Int -> Tree a -> String
drawTreeHelper i (Leaf h a) = indentNewLine (2*i) ++ showHash h ++ " " ++ show a
drawTreeHelper i (Node a l Empty) = indentNewLine (2*i)
                                    ++ showHash a
                                    ++ " +"
                                    ++ drawTreeHelper (i+1) l
drawTreeHelper i (Node a l r) = indentNewLine(2*i)
                                ++ showHash a
                                ++ " -"
                                ++ drawTreeHelper (i+1) l
                                ++ drawTreeHelper (i+1) r

-- drawTree :: Show a => Tree a -> String
-- drawTree Empty = []
-- drawTree t = drawTreeHelper 0 t ++ "\n"

-- drawTreeHelper :: Show a => Int -> Tree a -> String
-- drawTreeHelper i (Leaf h a) = indentNewLine (2*i) ++ showHash h ++ " " ++ show a
-- drawTreeHelper i (Node a l Empty) = indentNewLine (2*i)
--                                     ++ showHash a
--                                     ++ " +"
--                                     ++ drawTreeHelper (i+1) l
-- drawTreeHelper i (Node a l r) = indentNewLine(2*i)
--                                 ++ showHash a
--                                 ++ " -"
--                                 ++ drawTreeHelper (i+1) l
--                                 ++ drawTreeHelper (i+1) r

indentNewLine :: Int -> String
indentNewLine 0 = []
indentNewLine n = "\n" ++ replicate n ' '

-- part B
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
showMerklePath [] = ""
showMerklePath [Left a] = "<" ++ showHash a
showMerklePath [Right a] = ">" ++ showHash a
showMerklePath (p:ps) = showMerklePath [p] ++ showMerklePath ps

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof e t = do
  path <- maybeHead (merklePaths e t)
  return $ MerkleProof e path

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths e Empty = [] -- czy to potrzebne?
merklePaths e (Leaf h _)
    | hash e == h = [[]]
    | otherwise = []
merklePaths e (Node a l Empty)
    | hash e == treeHash l = [[Left $ treeHash l]] -- twig has duplicated child
    | otherwise = []
merklePaths e (Node a l r) = fmap (Left (treeHash r):) (merklePaths e l)
                          ++ fmap (Right (treeHash l):) (merklePaths e r)

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

>>> mapM_ print $ map showMerklePath $ merklePaths 'i' $ buildTree "bitcoin"
"<0x5214666a<0x7400b6ff>0x00000062"
">0x69f4387c<0x6e00ad98>0x0000006f"
-}





