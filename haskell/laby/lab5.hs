import Control.Monad.Reader
import Data.Map
import Prelude hiding (lookup)


-- task 1

-- allPairs :: [a] -> [a] -> [[a]]

-- task 2

-- 2a
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

renumber :: Tree a -> Tree Int
renumber t = renumberHelper t 0

renumberHelper :: Tree a -> Int -> Tree Int
renumberHelper Empty i = Empty
renumberHelper (Node a l r) i = Node i (renumberHelper l (i+1)) (renumberHelper r (i+1))

-- with Reader monad
renumber2 :: Tree a -> Tree Int
renumber2 t = runReader (renumber2Helper t) 0

renumber2Helper :: Tree a -> Reader Int (Tree Int)
renumber2Helper (Node a l r) = do
    cur_lvl <- ask
    left <- local (+1) $ renumber2Helper l
    right <- local (+1) $ renumber2Helper r
    return (Node cur_lvl left right)
renumber2Helper Empty = return Empty

-- 2 b
type Var = String
data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
     | ELet Var Exp Exp  -- let var = e1 in e2

data Op = OpAdd | OpMul | OpSub

type Env = Map Var Int

evalExp :: Exp -> Int
evalExp x = runReader (evalExpHelper x) (fromList [])

evalExpHelper :: Exp -> Reader Env Int
evalExpHelper (EInt x) = return x
evalExpHelper (EVar x) = do
    var <- asks (lookupVar x)
    return var
evalExpHelper (EOp op e1 e2) = do
    a <- local id $ evalExpHelper e1
    b <- local id $ evalExpHelper e2
    return $ doOp op a b
evalExpHelper (ELet var e1 e2) = do
    a <- local id $ evalExpHelper e1
    res <- local (insert var a) $ evalExpHelper e2
    return res

doOp :: Op -> Int -> Int -> Int
doOp OpAdd a b = a + b
doOp OpMul a b = a * b
doOp OpSub a b = a - b

lookupVar :: String -> Env -> Int
lookupVar name env = maybe 0 id (lookup name env)
