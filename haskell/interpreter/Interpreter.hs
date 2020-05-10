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
                    then throwError ModZero
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
        loc <- lookupVar a
        SArr array <- gets (lookupStore loc)
        SInt idx <- evalExpr e
        validateIndex array idx
        return $ array ! (fromInteger idx)


    --function expr

    --Iga: data TopDef = FnDef Type Ident [ArgOrRef] Block
    evalExpr (EApp fun rArgs) = do
        env <- ask
        
        let ((FnDef typ ident fArgs funBody), gvenv') = lookupFun fun (fEnv env)
        
        venv' <- local (\_ -> env {gEnv = gvenv'}) (mapArgs fArgs rArgs)
        
        (_, val, flag) <- local (\_ -> env {vEnv = venv', gEnv = gvenv'}) (execStmt $ BStmt funBody)
        case val of
            Just i -> return i
            Nothing -> return $ SInt 0 --Iga: tu poprawić


    mapArgs :: [ArgOrRef] -> [ExprOrRef] -> MM (VEnv)
    mapArgs [] [] = do
        env <- ask
        return $ vEnv env

    mapArgs (RefArg typ a:fArgs) (ERefArg b:rArgs) = do
        env <- ask
        loc <- lookupVar b
        let venv' = insertVar a loc (vEnv env)
        local (\_ -> env {vEnv = venv'}) (mapArgs fArgs rArgs)

    mapArgs (Arg typ a:fArgs) (EExpArg b:rArgs) = do
        env <- ask
        (s, loc) <- get
        modify (getNewLoc)
        let venv' = insertVar a loc (vEnv env)
        val <- evalExpr b
        loc <- local (\_ -> env {vEnv = venv'}) (lookupVar a)
        modify (insertStore loc val)
        local (\_ -> env {vEnv = venv'}) (mapArgs fArgs rArgs)

--------------------------------------------------
------------------ STATEMENTS --------------------
--------------------------------------------------
   --Iga: tu się będzie powtarzać
    execStmtHelper (BStmt (Block [])) = do
        env <- ask
        return (env, Nothing, FNothing)

    execStmtHelper (BStmt (Block (s:ss))) = do
        (env, val, flag) <- execStmt s
        case flag of
            FNothing -> local (\_ -> env) (execStmtHelper (BStmt (Block ss)))
            FReturn -> return (env, val, flag)
            FBreak -> return (env, val, flag)
            FContinue -> return (env, val, flag)

    execStmt :: Stmt -> MM (Env, Maybe StoredVal, Flag)    

    execStmt (BStmt (Block b)) = do
        env <- ask
        (_, val, flag) <- execStmtHelper (BStmt (Block b))
        return (env, val, flag)

    execStmt (Decl t (NoInit ident)) = do
        let e = getDefaultExpr t
        execStmt (Decl t (Init ident e))

    execStmt (Decl t (Init ident expr)) = do
        (s, loc) <- get
        modify (getNewLoc)
        env <- ask
        let venv' = insertVar ident loc (vEnv env)
        local (\_ -> env {vEnv = venv'}) (execStmt (Ass ident expr))
        return (env {vEnv = venv'}, Nothing, FNothing)

    execStmt (Decl t (ArrNoInit ident expr)) = do
        SInt size <- evalExpr expr
        let initList = replicate (fromInteger size) (getDefaultExpr t)
        execStmt (Decl t (ArrInit ident expr initList))

    execStmt (Decl t (ArrInit ident expr initList)) = do
        (s, loc) <- get
        modify (getNewLoc)
        env <- ask
        SInt size <- evalExpr expr
        if size < 0
            then throwError InvalidSize
            else do
                list <- mapM evalExpr initList
                arr <- storeArray size list
                modify (insertStore loc arr)
                let venv' = insertVar ident loc (vEnv env)
                return (env {vEnv = venv'}, Nothing, FNothing)

    execStmt (Ass ident e) = do
        val <- evalExpr e
        env <- ask
        loc <- lookupVar ident
        modify (insertStore loc val)
        return (env, Nothing, FNothing)
    
    execStmt (ArrAss ident idx_e e) = do
        SInt idx <- evalExpr idx_e
        val <- evalExpr e
        env <- ask
        loc <- lookupVar ident
        SArr array <- gets (lookupStore loc)
        validateIndex array idx
        let newArr = insert idx val array
        modify (insertStore loc (SArr newArr))
        return (env, Nothing, FNothing)

    execStmt (Ret e) = do
        env <- ask
        expr <- evalExpr e
        return (env, Just expr, FReturn)

    execStmt (VRet) = do
        env <- ask
        return (env, Nothing, FReturn)
    
    execStmt (Cond e b) = do
        env <- ask
        expr <- evalExpr e
        case expr of
            SBool True -> execStmt $ BStmt b
            SBool False -> return (env, Nothing, FNothing)

    execStmt (CondElse e if_b else_b) = do
        expr <- evalExpr e
        case expr of
            SBool True  -> execStmt $ BStmt if_b
            SBool False -> execStmt $ BStmt else_b

    execStmt (While e b) = do
        env <- ask
        expr <- evalExpr e
        case expr of
            SBool True -> do
                (env', val, flag) <- execStmt (Cond e b)
                case flag of
                    FNothing -> local(\_ -> env') (execStmt (While e b))
                    FReturn -> return (env', val, flag)
                    FBreak ->  return (env', val, FNothing)
                    FContinue -> local(\_ -> env') (execStmt (While e b))
            SBool False -> return (env, Nothing, FNothing)


    --Iga: tu dodać przerwanie returnem
    execStmt (For v start end (Block b)) = do
        (env, _, _) <- execStmt (Ass v start)
        let incr = Ass v (EAdd (EVar v) Plus (ELitInt 1)) in
            local(\_ -> env) (execStmt $ While (ERel (EVar v) LTH end) (Block (b ++ [incr])))

    
    execStmt (Print e) = do
        expr <- evalExpr e
        env <- ask
        case expr of
            SInt expr  -> do
                        liftIO $ putStrLn $ show expr
                        return (env, Nothing, FNothing)
            SBool expr -> do
                        liftIO $ putStrLn $ show expr
                        return (env, Nothing, FNothing)
            SStr expr  -> do
                        liftIO $ putStrLn $ show expr
                        return (env, Nothing, FNothing)
    
    execStmt (SExp e) = do
        env <- ask
        val <- evalExpr e
        return (env, Just val, FNothing)
    
    execStmt (Break) = do
        env <- ask
        return (env, Nothing, FBreak)
    
    execStmt (Cont) = do
        env <- ask
        return (env, Nothing, FContinue)

--------------------------------------------------
--------------------- RUN ------------------------
--------------------------------------------------

    interpret :: Program -> MM (StoredVal)
    interpret (Program []) = do
        env <- ask
        local (\_ -> env) (evalExpr (EApp (Ident "main") []))
    
    interpret (Program (f:fs)) = do 
                    env <- interpretTopDef f
                    local (\_ -> env) (interpret (Program fs))

    interpretTopDef :: TopDef -> MM (Env)
    interpretTopDef (GlobalVar typ item) = do
        env <- ask
        case item of
            (NoInit ident) -> do
                gvenv' <- insertGlobalVar ident
                return env {gEnv = gvenv'}
            (Init ident e) -> do
                val <- evalExpr e
                gvenv' <- insertGlobalVar ident
                env <- ask
                loc <- local (\_ -> env {gEnv = gvenv'}) (lookupVar ident)
                modify (insertStore loc val)
                return env {gEnv = gvenv'}

            (ArrNoInit ident e) -> do
                SInt size <- evalExpr e
                let initList = replicate (fromInteger size) (getDefaultExpr typ)
                (s, loc) <- get
                modify (getNewLoc)
                env <- ask
                if size < 0
                    then throwError InvalidSize
                    else do
                        list <- mapM evalExpr initList
                        arr <- storeArray size list
                        modify (insertStore loc arr)
                        let gvenv' = insertVar ident loc (gEnv env)
                        return env {gEnv = gvenv'}

            (ArrInit ident e initList) -> do
                SInt size <- evalExpr e
                (s, loc) <- get
                modify (getNewLoc)
                env <- ask
                if size < 0
                    then throwError InvalidSize
                    else do
                        list <- mapM evalExpr initList
                        arr <- storeArray size list
                        modify (insertStore loc arr)
                        let gvenv' = insertVar ident loc (gEnv env)
                        return env {gEnv = gvenv'}


    interpretTopDef (FnDef typ ident args block) = do
        env <- ask
        let fenv = insertFun ident ((FnDef typ ident args block), (gEnv env)) (fEnv env)
        return $ env {fEnv = fenv}

    
    runProg prog = runExceptT $ runStateT (runReaderT (interpret prog) initialEnv) (Map.empty, 0) --Iga: skopiowane



