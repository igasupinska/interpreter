module Interpreter where
    import AbsGramm

    import Control.Monad.Reader
    import Control.Monad.State.Lazy
    import Control.Monad.Except
    import Control.Exception
    import Data.Map as Map
    import Data.Maybe
    import Prelude hiding (lookup)
    
    import Types
    import Helper

    getNewLoc :: Store -> Store
    getNewLoc (store, lastLoc) = (store, lastLoc + 1)

    lookupVar :: Ident -> MM (Loc)
    lookupVar ident = do
        (venv, gvenv, _) <- ask
        if member ident venv
            then return $ venv ! ident
            else return $ gvenv ! ident

    insertVar :: Ident -> Loc -> VEnv -> VEnv
    insertVar name loc env = insert name loc env

    insertGlobalVar :: Ident -> MM (GEnv)
    insertGlobalVar ident = do
        (s, loc) <- get
        modify (getNewLoc)
        (_, gvenv, _) <- ask
        return $ insert ident loc gvenv


    lookupFun :: Ident -> FEnv -> TopDef
    lookupFun ident fenv = fenv ! ident

    insertFun :: Ident -> TopDef -> FEnv -> FEnv
    insertFun ident def fenv = insert ident def fenv

    lookupStore :: Loc -> Store -> StoredVal
    lookupStore loc (store, _) = store ! loc

    insertStore :: Loc -> StoredVal -> Store -> Store
    insertStore loc val (store, lastLoc) = (insert loc val store, lastLoc)

--------------------------------------------------
----------------- EXPRESSIONS --------------------
--------------------------------------------------

    evalExpr :: Expr -> MM (StoredVal)

    evalExpr (EVar ident) = do
        loc <- lookupVar ident
        val <- gets (lookupStore loc)
        return val
    
    --int expr
    evalExpr (ELitInt x) = return $ SInt x
    
    evalExpr (Neg e) = do
        SInt i <- evalExpr e
        return $ SInt (-i)

    evalExpr (EMul e1 op e2) = do
        SInt i1 <- evalExpr e1
        SInt i2 <- evalExpr e2
        case op of
            Times -> return $ SInt $ i1 * i2
            Div -> if i2 == 0
                    then throwError DivZero
                    else return $ SInt $ i1 `div` i2 
            Mod -> if i2 == 0
                    then throwError DivZero --Iga: może dodać ModZero
                    else return $ SInt $ i1 `mod` i2 

    evalExpr (EAdd e1 op e2) = do
        SInt i1 <- evalExpr e1
        SInt i2 <- evalExpr e2
        case op of
            Plus  -> return $ SInt $ i1 + i2
            Minus -> return $ SInt $ i1 - i2

    --string expr
    evalExpr (EString s) = return $ SStr s
    
    --bool expr
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

    --array expr
    evalExpr (ArrAcc a e) = do
        (venv, gvenv, fenv) <- ask
        loc <- lookupVar a
        SInt size <- gets (lookupStore loc)
        SInt idx <- evalExpr e
        if idx < size
            then do
                val <- gets(lookupStore (loc + 1 + idx)) -- Iga: ok
                return val
            else throwError OutOfBound

    --function expr

    --Iga: data TopDef = FnDef Type Ident [ArgOrRef] Block
    evalExpr (EApp fun rArgs) = do
        (venv, gvenv, fenv) <- ask
        let (FnDef typ ident fArgs funBody) = lookupFun fun fenv
        -- venv2 <- prepArgs fArgs
        venv3 <- local (\_ -> (venv, gvenv, fenv)) (mapArgs fArgs rArgs)
        (venv3, _, fenv3, val, flag) <- local (\_ -> (venv3, gvenv, fenv)) (execStmt $ BStmt funBody)
        case val of
            Just i -> return i
            Nothing -> return $ SInt 0 --Iga: tu poprawić

    mapArgs :: [ArgOrRef] -> [ExprOrRef] -> MM (VEnv)
    mapArgs [] [] = do
        (venv, _, _) <- ask
        return venv

    mapArgs (RefArg typ a:fArgs) (ERefArg b:rArgs) = do
        (venv, gvenv, fenv) <- ask
        loc <- lookupVar b
        let newEnv = insertVar a loc venv
        local (\_ -> (newEnv, gvenv, fenv)) (mapArgs fArgs rArgs)

    mapArgs (Arg (Arr typ) a:fArgs) (EExpArg b:rArgs) = do
        initList <- listFromArr b typ
        (s, loc) <- get 
        (venv, gvenv, fenv, _, _) <- execStmt (Decl typ (ArrInit a (ELitInt $ toInteger (length initList)) initList))
        let venv' = insertVar a loc venv
        local (\_ -> (venv', gvenv, fenv)) (mapArgs fArgs rArgs)

    mapArgs (Arg typ a:fArgs) (EExpArg b:rArgs) = do
        (venv, gvenv, fenv, _, _) <- execStmt (Decl typ (Init a b))
        loc <- local (\_ -> (venv, gvenv, fenv)) (lookupVar a)
        val <- evalExpr b
        modify (insertStore loc val)
        local (\_ -> (venv, gvenv, fenv)) (mapArgs fArgs rArgs)

    listFromArr :: Expr -> Type -> MM([Expr])
    listFromArr (EVar ident) t = do
        (venv, gvenv, fenv) <- ask
        loc <- lookupVar ident
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
        (venv, gvenv, fenv) <- ask
        return (venv, gvenv, fenv, Nothing, FNothing)

    execStmtHelper (BStmt (Block (s:ss))) = do
        (venv, gvenv, fenv, val, flag) <- execStmt s
        case flag of
            FNothing -> local (\_ -> (venv, gvenv, fenv)) (execStmtHelper (BStmt (Block ss)))
            FReturn -> return (venv, gvenv, fenv, val, flag)
            FBreak -> return (venv, gvenv, fenv, val, flag)
            FContinue -> return (venv, gvenv, fenv, val, flag)

    execStmt :: Stmt -> MM (VEnv, GEnv, FEnv, Maybe StoredVal, Flag)    

    execStmt (BStmt (Block b)) = do
        (venv, gvenv, fenv) <- ask
        (_, _, _, val, flag) <- local (\_ -> (venv, gvenv, fenv)) (execStmtHelper (BStmt (Block b)))
        return (venv, gvenv, fenv, val, flag)

    execStmt (Decl t (NoInit ident)) = do
        let e = getDefaultExpr t
        execStmt (Decl t (Init ident e))

    execStmt (Decl t (Init ident expr)) = do
        (s, loc) <- get
        modify (getNewLoc)
        (venv, gvenv, fenv) <- ask
        let newVenv = insertVar ident loc venv
        local (\_ -> (newVenv, gvenv, fenv)) (execStmt (Ass ident expr))
        return (newVenv, gvenv, fenv, Nothing, FNothing)

    execStmt (Decl t (ArrNoInit ident expr)) = do
        (s, loc) <- get
        modify (getNewLoc)
        (venv, gvenv, fenv) <- ask
        SInt size <- evalExpr expr
        modify (insertStore loc (SInt size))
        val <- evalExpr $ getDefaultExpr t
        () <- storeArray size (replicate (fromInteger size) (ELitInt 0)) --Iga: tu poprawić
        return (insertVar ident loc venv, gvenv, fenv, Nothing, FNothing)

    execStmt (Decl t (ArrInit ident expr initList)) = do
        (s, loc) <- get
        modify (getNewLoc)
        (venv, gvenv, fenv) <- ask
        SInt size <- evalExpr expr
        modify (insertStore loc (SInt size))
        storeArray size initList
        return (insertVar ident loc venv, gvenv, fenv, Nothing, FNothing)

    execStmt (Ass ident e) = do
        val <- evalExpr e
        (venv, gvenv, fenv) <- ask
        loc <- lookupVar ident
        modify (insertStore loc val)
        return (venv, gvenv, fenv, Nothing, FNothing)
    
    execStmt (ArrAss ident idx_e e) = do
        SInt idx <- evalExpr idx_e
        val <- evalExpr e
        (venv, gvenv, fenv) <- ask
        loc <- lookupVar ident
        SInt size <- gets (lookupStore loc)
        if idx < size
        then do
            modify (insertStore (loc + 1 + idx) val)
            return (venv, gvenv, fenv, Nothing, FNothing)
        else throwError OutOfBound

    execStmt (Ret e) = do
        (venv, gvenv, fenv) <- ask
        expr <- evalExpr e
        return (venv, gvenv, fenv, Just expr, FReturn)

    execStmt (VRet) = do
        (venv, gvenv, fenv) <- ask
        return (venv, gvenv, fenv, Nothing, FReturn)
    
    execStmt (Cond e b) = do
        (venv, gvenv, fenv) <- ask
        expr <- evalExpr e
        case expr of
            SBool True -> execStmt $ BStmt b
            SBool False -> return (venv, gvenv, fenv, Nothing, FNothing)

    execStmt (CondElse e if_b else_b) = do
        expr <- evalExpr e
        case expr of
            SBool True  -> execStmt $ BStmt if_b
            SBool False -> execStmt $ BStmt else_b

    execStmt (While e b) = do
        (venv, gvenv, fenv) <- ask
        expr <- evalExpr e
        case expr of
            SBool True -> do
                (venv2, gvenv2, fenv2, val, flag) <- execStmt (Cond e b)
                case flag of
                    FNothing -> local(\_ -> (venv2, gvenv2, fenv2)) (execStmt (While e b))
                    FReturn -> return (venv2, gvenv2, fenv2, val, flag)
                    FBreak ->  return (venv2, gvenv2, fenv2, val, FNothing)
                    FContinue -> local(\_ -> (venv2, gvenv2, fenv2)) (execStmt (While e b))
            SBool False -> return (venv, gvenv, fenv, Nothing, FNothing)


    --Iga: tu dodać przerwanie returnem
    execStmt (For v start end (Block b)) = do
        (venv, gvenv, fenv, _, _) <- execStmt (Ass v start)
        let incr = Ass v (EAdd (EVar v) Plus (ELitInt 1)) in
            local(\_ -> (venv, gvenv, fenv)) (execStmt $ While (ERel (EVar v) LTH end) (Block (b ++ [incr])))

    
    execStmt (Print e) = do
        expr <- evalExpr e
        (venv, gvenv, fenv) <- ask
        case expr of
            SInt expr  -> do
                        liftIO $ putStrLn $ show expr
                        return (venv, gvenv, fenv, Nothing, FNothing)
            SBool expr -> do
                        liftIO $ putStrLn $ show expr
                        return (venv, gvenv, fenv, Nothing, FNothing)
            SStr expr  -> do
                        liftIO $ putStrLn $ show expr
                        return (venv, gvenv, fenv, Nothing, FNothing)
    
    execStmt (SExp e) = do
        (env, gvenv, fenv) <- ask
        val <- evalExpr e
        return (env, gvenv, fenv, Just val, FNothing)
    
    execStmt (Break) = do
        (venv, gvenv, fenv) <- ask
        return (venv, gvenv, fenv, Nothing, FBreak)
    
    execStmt (Cont) = do
        (venv, gvenv, fenv) <- ask
        return (venv, gvenv, fenv, Nothing, FContinue)

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

    --Iga: przenazwać
    runFunction :: TopDef -> MM (VEnv, GEnv, FEnv)
    runFunction (GlobalVar typ item) = do
        (venv, gvenv, fenv) <- ask
        case item of
            (NoInit id@(Ident ident)) -> do
                gvenv' <- insertGlobalVar id
                return (venv, gvenv', fenv)
            (Init id@(Ident ident) e) -> do
                val <- evalExpr e
                gvenv' <- insertGlobalVar id
                (venv, gvenv, fenv) <- ask
                loc <- local (\_ -> (venv, gvenv', fenv)) (lookupVar id)
                modify (insertStore loc val)
                return (venv, gvenv', fenv)
            -- (ArrNoInit id@(Ident ident) _) -> do
            --     gvenv' <- insertGlobalVar id (Arr typ)
            --     return (venv, gvenv', fenv)
            -- (ArrInit id@(Ident ident) _ _) -> do
            --     gvenv' <- insertGlobalVar id (Arr typ)
            --     return (venv, gvenv', fenv)

    runFunction (FnDef typ ident args block) = do
        (venv, gvenv, fenv) <- ask
        -- venv2 <- prepArgs args
        return $ (venv, gvenv, insertFun ident (FnDef typ ident args block) fenv)     

    
    runProg prog = runExceptT $ runStateT (runReaderT (runProgram prog) (Map.empty, Map.empty, Map.empty)) (Map.empty, 0) --Iga: skopiowane



