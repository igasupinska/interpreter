module Blockchain where
import Control.Monad
import Data.Word

import Hashable32
import HashTree
import PPrint
import Utils

type Address = Hash
type Amount = Word32
coin :: Amount
coin = 1000
data Transaction = Tx
  { txFrom :: Address
  , txTo :: Address
  , txAmount :: Amount
  } deriving Show

instance Hashable Transaction where
  hash (Tx a b c) = hash [hash a, hash b, hash c]

data Block = Block { blockHdr :: BlockHeader, blockTxs ::  [Transaction]}

instance Show Block where
  show (Block hdr txs) = unlines (show hdr : map show txs)

instance Hashable Block where
  hash = hash . blockHdr

data BlockHeader = BlockHeader
  {
    parent :: Hash
  , coinbase :: Transaction
  , txroot :: Hash -- root of the Merkle tree
  , nonce :: Hash
  } deriving Show

instance Hashable BlockHeader where
  hash (BlockHeader p c r n) = hash [p,hash c, r, n]

difficulty = 5
blockReward = 50*coin
coinbaseTx miner = Tx {txFrom = 0, txTo = miner, txAmount = blockReward}

validNonce :: BlockHeader -> Bool
validNonce b = (hash b) `mod` (2^difficulty) == 0

tx1 = Tx
  { txFrom = hash "Alice"
  , txTo = hash "Bob"
  , txAmount = 1*coin
  }

type Miner = Address
type Nonce = Word32

mineBlock :: Miner -> Hash -> [Transaction] -> Block
mineBlock miner parent txs = mineBlockHelper 0 BlockHeader
                                                  { parent = parent
                                                  , coinbase = coinbaseTx miner
                                                  , txroot = treeHash $ buildTree (coinbaseTx miner:txs)
                                                  , nonce = hash (0 :: Nonce)
                                                  } txs

mineBlockHelper :: Word32 -> BlockHeader -> [Transaction] -> Block
mineBlockHelper num hdr txs = if validNonce hdr
                                      then (Block hdr txs)
                                      else mineBlockHelper (num + 1) hdr {nonce = hash (num +1)} txs

genesis = block0
block0 = mineBlock (hash "Satoshi") 0 []
block1 = mineBlock (hash "Alice") (hash genesis) []
block2 = mineBlock (hash "Charlie") (hash block1) [tx1]
chain = [block2, block1, block0]

-- | Chain verification
-- >>> verifyChain [block1, block2]
-- Nothing
--
-- >>> VH <$> verifyChain [block2,block1,block0]
-- Just 0x0dbea380

-- do przepisania
validChain :: [Block] -> Bool
validChain [] = False -- co tutaj?
validChain [b] = isJust (verifyBlock b 0)
validChain (b1:b2:bs) = isJust (verifyBlock b1 (hash b2)) && validChain bs

verifyChain :: [Block] -> Maybe Hash
verifyChain b@(b1:bs) = do
  guard (validChain b)
  return (hash b1)

verifyBlock :: Block -> Hash -> Maybe Hash
verifyBlock b@(Block hdr txs) parentHash = do
  guard (parent hdr == parentHash)
  guard (txroot hdr == treeHash (buildTree (coinbase hdr:txs)))
  return (hash b)


{- | Transaction Receipts
>>> let charlie = hash "Charlie"
>>> let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1]
>>> block
BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2327748117, nonce = 3}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}
<BLANKLINE>

>>> receipt
TxReceipt {txrBlock = 230597504, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0xbcc3e45a}
>>> validateReceipt receipt (blockHdr block)
True
-}

data TransactionReceipt = TxReceipt
  {  txrBlock :: Hash, txrProof :: MerkleProof Transaction } deriving Show

-- Iga: można uznać, że pełna funkcja
validateReceipt :: TransactionReceipt -> BlockHeader -> Bool
validateReceipt r hdr = txrBlock r == hash hdr
                        && verifyProof (txroot hdr) (txrProof r)

createReceipt :: Block -> Transaction -> TransactionReceipt
createReceipt (Block hdr txs) tx = TxReceipt {
                                txrBlock = hash hdr
                              , txrProof = fromMaybe (MerkleProof tx []) (buildProof tx (buildTree (coinbase hdr:txs)))
                              }

mineTransactions :: Miner -> Hash -> [Transaction] -> (Block, [TransactionReceipt])
mineTransactions miner parent txs = let b = mineBlock miner parent txs in
                                    (b, map (createReceipt b) txs)

{- | Pretty printing
>>> runShows $ pprBlock block2
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000

>>> runShows $ pprListWith pprBlock [block0, block1, block2]
hash: 0x70b432e0
parent: 0000000000
miner: 0x7203d9df
root: 0x5b10bd5d
nonce: 18
Tx# 0x5b10bd5d from: 0000000000 to: 0x7203d9df amount: 50000
hash: 0x2f83ae40
parent: 0x70b432e0
miner: 0x790251e0
root: 0x5ea7a6f0
nonce: 0
Tx# 0x5ea7a6f0 from: 0000000000 to: 0x790251e0 amount: 50000
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000
-}
pprHeader :: BlockHeader -> ShowS
pprHeader self@(BlockHeader parent cb txroot nonce)
  = pprV [ p ("hash", VH $ hash self)
         , p ("parent", VH $ parent)
         , p ("miner", VH $ txTo cb)
         , p ("root", VH txroot)
         , p ("nonce", nonce)
         ]
  where
    nl = showString "\n"
    p :: Show a => (String, a) -> ShowS
    p = showsPair

pprBlock :: Block -> ShowS
pprBlock (Block header txs)
 = pprHeader header
 . showString "\n"
 . pprTxs (coinbase header:txs)

pprTx :: Transaction -> ShowS
pprTx tx@(Tx from to amount)
  = pprH [ showString "Tx#"
         , showsHash (hash tx)
         , p ("from", VH from)
         , p ("to", VH to)
         , p ("amount", amount)
         ]
  where
    p :: Show a => (String, a) -> ShowS
    p = showsPair

pprTxs :: [Transaction] -> ShowS
pprTxs = pprV . map pprTx


-- usunąć, moje testy
{- |
>>> runShows $ pprBlock $ mineBlock (hash "Charlie") (hash block1) [tx1]
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000

>>> runShows $ pprBlock block0
hash: 0x70b432e0
parent: 0000000000
miner: 0x7203d9df
root: 0x5b10bd5d
nonce: 18
Tx# 0x5b10bd5d from: 0000000000 to: 0x7203d9df amount: 50000

>>> runShows $ pprBlock block1
hash: 0x2f83ae40
parent: 0x70b432e0
miner: 0x790251e0
root: 0x5ea7a6f0
nonce: 0
Tx# 0x5ea7a6f0 from: 0000000000 to: 0x790251e0 amount: 50000

>>> makeTx f t a = Tx (hash f) (hash t) (a*coin)
>>> tx1 = makeTx "Satoshi" "Alice" 10
>>> tx2 = makeTx "Alice" "Bob" 1
>>> tx3 = makeTx "Alice" "Charlie" 1
>>> mineTransactions (hash "Charlie") (hash block1) [tx1,tx2,tx3]
(BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2996394280, nonce = 26}
Tx {txFrom = 1912855007, txTo = 2030195168, txAmount = 10000}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}
Tx {txFrom = 2030195168, txTo = 1392748814, txAmount = 1000}
,[TxReceipt {txrBlock = 3725795968, txrProof = MerkleProof (Tx {txFrom = 1912855007, txTo = 2030195168, txAmount = 10000}) <0xae9d56b7>0xbcc3e45a},TxReceipt {txrBlock = 3725795968, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0x3c177e6b<0x1b6a0892},TxReceipt {txrBlock = 3725795968, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 1392748814, txAmount = 1000}) >0x3c177e6b>0x085e2467}])

>>> runShows $ pprListWith showString ["asd", "zxc"]
asd
zxc
-}