module Interpreter where
    import AbsGramm

    import Control.Monad.Reader
    import Control.Monad.State.Lazy
    import Data.Map as Map
    import Data.Maybe
    import Prelude hiding (lookup)
    
    data StoredVal = SInt Integer | SStr String | SBool Bool
    type Loc = Integer
    type Store = (Map Loc StoredVal, Loc)
    type VEnv = Map Ident Loc
    type FEnv = Map Ident (TopDef, VEnv)

    getNewLoc :: Store -> Store
    getNewLoc (store, lastLoc) = (store, lastLoc + 1)

    lookupVar :: Ident -> VEnv -> Loc
    lookupVar name env = fromMaybe 99 (lookup name env)

    insertVar :: Ident -> Loc -> VEnv -> VEnv
    insertVar name loc env = insert name loc env

-- Iga: data TopDef = FnDef Type Ident [ArgOrRef] Block
--Iga: tu zmienić
    lookupFun :: Ident -> FEnv -> (TopDef, VEnv)
    lookupFun ident fenv = fromMaybe ((FnDef Int (Ident "funkcja") [] (Block [])), Map.empty) (lookup ident fenv)

    insertFun :: Ident -> (TopDef, VEnv) -> FEnv -> FEnv
    insertFun ident def fenv = insert ident def fenv

    lookupStore :: Loc -> Store -> StoredVal
    lookupStore loc (store, _) = fromMaybe (SStr $ "Not found") (lookup loc store)

    insertStore :: Loc -> StoredVal -> Store -> Store
    insertStore loc val (store, lastLoc) = (insert loc val store, lastLoc)

    --Iga: data Type = Int | Str | Bool | Void | Arr Type
    --Iga: co z arr i void?
    getDefaultExpr :: Type -> Expr
    getDefaultExpr Int = ELitInt 0
    getDefaultExpr Str = EString []
    getDefaultExpr Bool = ELitFalse

--------------------------------------------------
----------------- EXPRESSIONS --------------------
--------------------------------------------------

    evalExpr :: Expr -> MM (StoredVal)

    evalExpr (EVar ident) = do
        (venv, fenv) <- ask
        val <- gets (lookupStore (lookupVar ident venv))
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

    --Iga: data TopDef = FnDef Type Ident [ArgOrRef] Block

    evalExpr (EApp fun rArgs) = do
        (venv, fenv) <- ask
        let ((FnDef typ ident fArgs funBody), venv2) = lookupFun fun fenv
        venv3 <- mapArgs fArgs rArgs venv2
        (venv3, fenv3, val) <- local (\_ -> (venv3, fenv)) (execStmt $ BStmt funBody)
        case val of
            Just i -> return i
            Nothing -> return $ SInt 0 --Iga: tu poprawić

    --array expr
    evalExpr (ArrAcc a e) = do
        (venv, fenv) <- ask
        let loc = lookupVar a venv
        SInt size <- gets (lookupStore loc)
        SInt idx <- evalExpr e
        if idx < size
            then do
                val <- gets(lookupStore (loc + 1 + idx)) -- Iga: ok
                return val
            else return $ SInt (-1) -- Iga: out of bound exception

    mapArgs :: [ArgOrRef] -> [ExprOrRef] -> VEnv -> MM (VEnv)
    mapArgs [] [] venv = return venv

    mapArgs (RefArg typ a:fArgs) (ERefArg b:rArgs) venv2 = do
        (venv, fenv) <- ask
        let loc = lookupVar b venv
        let newEnv = insertVar b loc venv2
        mapArgs fArgs rArgs newEnv

    mapArgs (Arg (Arr typ) a:fArgs) (EExpArg b:rArgs) venv = do
        initList <- listFromArr b typ
        (s, loc) <- get 
        (venv2, fenv, _) <- execStmt (Decl typ (ArrInit a (ELitInt $ toInteger (length initList)) initList))
        let venv3 = insertVar a loc venv2
        mapArgs fArgs rArgs venv3

    mapArgs (Arg typ a:fArgs) (EExpArg b:rArgs) venv2 = do
        (venv, fenv) <- ask
        val <- evalExpr b
        let loc = lookupVar a venv2
        modify (insertStore loc val)
        mapArgs fArgs rArgs venv2


    listFromArr :: Expr -> Type -> MM([Expr])
    listFromArr (EVar ident) t = do
        (venv, fenv) <- ask
        let loc = lookupVar ident venv
        SInt size <- gets (lookupStore loc)
        newArr <- getArr t (loc + 1) size
        return newArr

    getArr :: Type -> Loc -> Integer -> MM([Expr])
    getArr t l 0 = return []

    getArr t l s = do
        el <- gets (lookupStore l)
        rest <- getArr t (l+1) (s-1)
        case el of
            SInt x -> return $ (ELitInt x):rest
            SBool False -> return $ (ELitFalse):rest
            SBool True -> return $ (ELitTrue):rest
            SStr x -> return $ (EString x):rest


    storeArray :: Integer -> [Expr] -> MM()
    storeArray 0 val = return ()

    storeArray size (v:vs) = do
        (s, loc) <- get
        modify (getNewLoc)
        val <- evalExpr v
        modify (insertStore loc val)
        storeArray (size-1) vs

--------------------------------------------------
------------------ STATEMENTS --------------------
--------------------------------------------------
   --Iga: tu się będzie powtarzać
    execStmtHelper (BStmt (Block [])) = do
        (venv, fenv) <- ask
        return (venv, fenv, Nothing)

    execStmtHelper (BStmt (Block [s])) = do
        (venv, fenv) <- ask
        local (\_ ->(venv, fenv)) (execStmt s)

    execStmtHelper (BStmt (Block (s:ss))) = do
        (venv, fenv, val) <- execStmt s
        local (\_ -> (venv, fenv)) (execStmtHelper (BStmt (Block ss)))

    execStmt :: Stmt -> MM (VEnv, FEnv, Maybe StoredVal)    

    execStmt (BStmt (Block b)) = do
        (venv, fenv) <- ask
        (_, _, val) <- local (\_ -> (venv, fenv)) (execStmtHelper (BStmt (Block b)))
        return (venv, fenv, val)

    execStmt (Decl t (NoInit ident)) = do
        let e = getDefaultExpr t
        execStmt (Decl t (Init ident e))

    execStmt (Decl t (Init ident expr)) = do
        (s, loc) <- get
        modify (getNewLoc)
        (venv, fenv) <- ask
        let newVenv = insertVar ident loc venv
        local (\_ -> (newVenv, fenv)) (execStmt (Ass ident expr))
        return (newVenv, fenv, Nothing)

    execStmt (Decl t (ArrNoInit ident expr)) = do
        (s, loc) <- get
        modify (getNewLoc)
        (venv, fenv) <- ask
        SInt size <- evalExpr expr
        modify (insertStore loc (SInt size))
        val <- evalExpr $ getDefaultExpr t
        () <- storeArray size (replicate (fromInteger size) (ELitInt 0)) --Iga: tu poprawić
        return (insertVar ident loc venv, fenv, Nothing)

    execStmt (Decl t (ArrInit ident expr initList)) = do
        (s, loc) <- get
        modify (getNewLoc)
        (venv, fenv) <- ask
        SInt size <- evalExpr expr
        modify (insertStore loc (SInt size))
        storeArray size initList
        return (insertVar ident loc venv, fenv, Nothing)

    execStmt (Ass ident e) = do
        val <- evalExpr e
        (venv, fenv) <- ask
        modify (insertStore (lookupVar ident venv) val)
        execStmt VRet --Iga:tymczasowo
    
    execStmt (ArrAss ident idx_e e) = do
        SInt idx <- evalExpr idx_e
        val <- evalExpr e
        (venv, fenv) <- ask
        let loc = lookupVar ident venv
        modify (insertStore (loc + 1 + idx) val)
        execStmt VRet --Iga:tymczasowo


    execStmt (Ret e) = do
        (venv, fenv) <- ask
        expr <- local(\_ -> (venv, fenv)) (evalExpr e)
        return (venv, fenv, Just expr)

    execStmt (VRet) = do
        (venv, fenv) <- ask
        return (venv, fenv, Nothing)
    
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

    execStmt (For v start end (Block b)) = do
        (venv, fenv, _) <- execStmt (Ass  v start)
        let incr = Ass v (EAdd (EVar v) Plus (ELitInt 1)) in
            local(\_ -> (venv, fenv)) (execStmt $ While (ERel (EVar v) LTH end) (Block (incr:b)))

    
    execStmt (Print e) = do
        expr <- evalExpr e
        case expr of
            SInt expr  -> do
                        liftIO $ putStrLn $ show expr
                        execStmt VRet --Iga: tymczasowo
            SBool expr -> do
                        liftIO $ putStrLn $ show expr
                        execStmt VRet --Iga: tymczasowo
            SStr expr  -> do
                        liftIO $ putStrLn $ show expr
                        execStmt VRet --Iga: tymczasowo
    
    execStmt (SExp e) = do
        (env, fenv) <- ask
        val <- evalExpr e
        return (env, fenv, Just val)
    
    execStmt (Break) = undefined
    execStmt (Cont) = undefined


--------------------------------------------------
--------------------- RUN ------------------------
--------------------------------------------------

    runProgram :: Program -> MM (StoredVal)
    runProgram (Program []) = do
        env <- ask
        local (\_ -> env) (evalExpr (EApp (Ident "main") []))
    
    runProgram (Program (f:fs)) = do 
                    env <- runFunction f
                    local (\_ -> env) (runProgram (Program fs))

    prepArgs :: [ArgOrRef] -> MM (VEnv)
    prepArgs [] = do
        (venv, fenv) <- ask
        return venv
    prepArgs (RefArg typ a:as) = do
        (venv, fenv) <- ask
        local (\_ -> (venv, fenv)) (prepArgs as)
    prepArgs (Arg (Arr t) a:as) = do
        (venv, fenv) <- ask
        (venv2, fenv2, _) <- execStmt $ Decl t (ArrNoInit a (ELitInt 0)) --Iga: tu źle na maksa! jaki rozmiar tablicy?
        local (\_ -> (venv2, fenv)) (prepArgs as)
    prepArgs (Arg typ a:as) = do
        (venv, fenv) <- ask
        (venv2, fenv2, _) <- execStmt $ (Decl typ (NoInit a))
        local (\_ -> (venv2, fenv)) (prepArgs as)

    runFunction :: TopDef -> MM (VEnv, FEnv)
    runFunction (FnDef typ ident args block) = do
        (venv, fenv) <- ask
        venv2 <- prepArgs args
        return $ (venv, insertFun ident ((FnDef typ ident args block), venv2) fenv)     

    
    runProg prog = runStateT (runReaderT (runProgram prog) (Map.empty, Map.empty)) (Map.empty, 0) --Iga: skopiowane

    --my monad
    type MM = ReaderT (VEnv, FEnv) (StateT Store IO)