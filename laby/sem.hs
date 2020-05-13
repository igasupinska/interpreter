import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Map
import Prelude hiding (lookup)

-----------------------------------------------
-- TINY with expressions, basic statements 
-----------------------------------------------

type Var = String
data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
     | ELet Var Exp Exp  -- let var = e1 in e2

data Op = OpAdd | OpMul | OpSub

type Env = Map Var Int

evalExp :: Exp -> Store -> Int
evalExp x s = runReader (evalExpHelper x) s

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

data Stmt = Skip
        | Var := Exp
        | If Exp Stmt Stmt
        | While Exp Stmt
        | MySeq [Stmt] -- S1;S2

data Skip = Nop

execStmt :: Stmt -> IO ()
execStmt s = putStrLn $ printStore $ toList $ execState (execStmtHelper s) (fromList [])

printStore :: [(Var, Int)] -> String
printStore [] = []
printStore ((k, v):xs) = (show k) ++ " = " ++ (show v) ++ "\n"
    ++ (printStore xs)

type Store = Map Var Int

execStmtHelper :: Stmt -> State Store ()
execStmtHelper Skip = do
    return ()
execStmtHelper (x := e) = do
    s <- get
    modify (insert x (evalExp e s))
    return ()
execStmtHelper (If e s1 s2) = do
    s <- get
    let val = evalExp e s in
        if val /= 0
            then execStmtHelper s1
            else execStmtHelper s2
execStmtHelper (While e s1) = do
    s <- get
    let val = evalExp e s in
        if val /= 0
            then execStmtHelper (MySeq [s1, While e s1])
            else execStmtHelper Skip
execStmtHelper (MySeq [s]) = execStmtHelper s 
execStmtHelper (MySeq (s:ss)) = execStmtHelper s >> execStmtHelper (MySeq ss)

-- execStmt $ MySeq ["y" := (EInt 10), "x" := (EOp OpMul (EVar "y") (EInt 2)), While (EVar "y") ("y" := (EOp OpSub (EVar "y") (EInt 1)))]
