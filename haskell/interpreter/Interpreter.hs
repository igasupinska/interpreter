module Interpreter where
    import AbsGramm

    import Control.Monad.Reader
    import Control.Monad.State.Lazy
    import Data.Map as Map
    import Data.Maybe
    import Prelude hiding (lookup)
    
    data StoredVal = SInt Integer | SStr String | SBool Bool -- Iga: co z tablicami? Iga: czy to nie widzne, ze piszesz ze soba?

    type Loc = Integer
    type Store = Map Loc StoredVal
    type Env = Map Ident Loc

    lookupVar :: Ident -> Env -> Loc
    lookupVar name env = maybe 0 id (lookup name env)

    insertVar :: Ident -> Loc -> Env -> Env
    insertVar name loc env = insert name loc env

    lookupStore :: Loc -> Store -> StoredVal
    lookupStore loc store = fromMaybe (SInt 0) (lookup loc store)

    insertStore :: Loc -> StoredVal -> Store -> Store
    insertStore loc val store = insert loc val store

--------------------------------------------------
----------------- EXPRESSIONS --------------------
--------------------------------------------------

    evalExpr :: Expr -> MM (StoredVal)

    evalExpr (EVar ident) = do
        loc <- asks (lookupVar ident)
        val <- gets (lookupStore loc)
        return val
    
    --int expr
    evalExpr (ELitInt x) = return $ SInt x
    
    evalExpr (Neg e) = do
        SInt i <- evalExpr e
        return $ SInt (-i)
    
    evalExpr (EMul e1 Times e2) = do
        SInt i1 <- evalExpr e1
        SInt i2 <- evalExpr e2
        return $ SInt $ i1 * i2

    evalExpr (EMul e1 Div e2) = do
        SInt i1 <- evalExpr e1
        SInt i2 <- evalExpr e2
        if i2 == 0
            then return $ SStr "Division by zero!" --Iga: tu poprawić
            else return $ SInt $ i1 `div` i2 

    evalExpr (EMul e1 Mod e2) = do
        SInt i1 <- evalExpr e1
        SInt i2 <- evalExpr e2
        if i2 == 0
            then return $ SStr "Division by zero!" --Iga: tu poprawić
            else return $ SInt $ i1 `mod` i2 
    
    evalExpr (EAdd e1 Plus e2) = do
        SInt i1 <- evalExpr e1
        SInt i2 <- evalExpr e2
        return $ SInt $ i1 + i2
    
    evalExpr (EAdd e1 Minus e2) = do
        SInt i1 <- evalExpr e1
        SInt i2 <- evalExpr e2
        return $ SInt $ i1 - i2

    --string expr
    evalExpr (EString s) = return $ SStr s
    
    -- --bool expr
    evalExpr (ELitTrue) = return $ SBool True
    
    evalExpr (ELitFalse) = return $ SBool False
    
    evalExpr (Not e) = do
        SBool expr <- evalExpr e
        return $ SBool $ not expr
    
    evalExpr (ERel e1 op e2) = do
        SInt i1 <- evalExpr e1
        SInt i2 <- evalExpr e2
        case op of
            LTH -> return $ SBool $ i1 < i2 
            LE  -> return $ SBool $ i1 <= i2 
            GTH -> return $ SBool $ i1 > i2 
            GE  -> return $ SBool $ i1 >= i2 
            EQU -> return $ SBool $ i1 == i2 
            NE  -> return $ SBool $ i1 /= i2 

    evalExpr (EAnd e1 e2) = do
        SBool b1 <- evalExpr e1
        SBool b2 <- evalExpr e2
        return $ SBool $ b1 && b2
    
    evalExpr (EOr e1 e2) = do
        SBool b1 <- evalExpr e1
        case b1 of
            True -> return $ SBool True
            False -> evalExpr e2

    -- --function expr
    -- evalExpr (EApp fun (ERefArg a:as)) = undefined
    -- evalExpr (EApp fun (EExpArg a:as)) = undefined

    -- --array expr
    -- evalExprHelper (ArrAcc a e) = undefined


--------------------------------------------------
------------------ STATEMENTS --------------------
--------------------------------------------------

    execStmt :: Stmt -> MM (Maybe StoredVal)    
    execStmt (BStmt (Block [s])) = execStmt s
    execStmt (BStmt (Block (s:ss))) =
        (execStmt s) >> execStmt (BStmt (Block ss))
    
    execStmt (Decl t (NoInit ident)) = do
        -- loc <- 1 --Iga: powinna być jakaś funkcja newloc
        asks (insertVar ident 1)
        execStmt VRet --Iga:tymczasowo
   
    execStmt (Decl t (Init ident expr)) = do
        execStmt (Decl t (NoInit ident))
        execStmt (Ass ident expr)

    execStmt (Ass ident e) = do
        val <- evalExpr e
        loc <- asks (lookupVar ident)
        modify (insertStore loc val)
        execStmt VRet --Iga:tymczasowo
    
    execStmt (Ret e) = do
        expr <- evalExpr e
        return $ Just expr

    execStmt (VRet) = return Nothing
    
    execStmt (Cond e b) = do
        expr <- evalExpr e
        case expr of
            SBool True -> execStmt (BStmt b)
            SBool False -> execStmt VRet --tymczasowo
    
    execStmt (CondElse e if_b else_b) = do
        expr <- evalExpr e
        case expr of
            SBool True  -> execStmt $ BStmt if_b
            SBool False -> execStmt $ BStmt else_b

    execStmt (While e b) = do
        expr <- evalExpr e
        case expr of
            SBool True -> execStmt (Cond e b) >> execStmt (While e b)
            SBool False -> execStmt VRet

    execStmt (For v start end b) = undefined
    
    execStmt (Print e) = do
        expr <- evalExpr e
        case expr of
            SInt expr  -> do
                        -- liftIO $ putStrLn "Print int"
                        execStmt VRet --Iga: tymczasowo
            SBool expr ->
                        -- liftIO $ putStrLn "Print bool"
                        execStmt VRet --Iga: tymczasowo
            SStr expr  ->
                        -- liftIO $ putStrLn "print string"
                        execStmt VRet --Iga: tymczasowo
    
    execStmt (SExp e) = undefined
    execStmt (Break) = undefined
    execStmt (Cont) = undefined


--------------------------------------------------
--------------------- RUN ------------------------
--------------------------------------------------

    interpretFun :: Program -> MM (Maybe StoredVal)
    interpretFun (Program [FnDef Int f as (Block s)]) = interpret s
    interpretFun (Program (d:ds)) = do 
                    interpretFun (Program [d])
                    interpretFun (Program ds)

    interpret :: [Stmt] -> MM (Maybe StoredVal)
    interpret [s] = execStmt s
    interpret (s:xs) = do
        execStmt s
        interpret xs
    
    runProg prog = runState (runReaderT (interpretFun prog) Map.empty) (Map.empty) --Iga: skopiowane

    -- evalExpr :: Expr -> Store -> StoredVal
    -- evalExpr x s = runReader (evalExprHelper x) s

    --my monad
    type MM = ReaderT Env (State Store)