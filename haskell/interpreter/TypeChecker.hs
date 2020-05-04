module TypeChecker where
    import AbsGramm

    import Control.Monad.Reader
    import Control.Monad.State.Lazy
    import Control.Monad.Except
    import Control.Exception
    import Data.Map as Map
    import Data.Maybe
    import Prelude hiding (lookup)
    
--------------------------------------------------
--------------------- ENV ------------------------
--------------------------------------------------
    --types monad
    type TM = ReaderT EnvT (ExceptT String IO)

    --variable types environment
    type VEnvT = Map Ident Type
    
    --(return type, args type, environments)
    type FnDefT = (Type, [ArgType], [VEnvT])

    --function types environment
    type FEnvT = Map Ident FnDefT

    --list of venv makes for block visible venv, the most nested one first
    type EnvT = ([VEnvT], FEnvT)

    --keeps type of argument and info whether its reference or not
    --(typ, isRef)
    type ArgType = (Ident, Type, Bool)


    lookupVar :: Ident -> [VEnvT] -> TM (Type)
    lookupVar ident [] = throwError ("Variable " ++ show ident ++ " not declared")

    lookupVar ident (e:es) =
        if member ident e
            then return $ fromMaybe (Int) (lookup ident e) --Iga: poprawić
            else lookupVar ident es

    insertVar :: Ident -> Type -> [VEnvT] -> [VEnvT]
    insertVar name typ (e:es) = insert name typ e:es

-- Iga: data TopDef = FnDef Type Ident [ArgOrRef] Block
--Iga: tu zmienić fromMaybe
    lookupFun :: Ident -> FEnvT -> FnDefT
    lookupFun ident fenv = fromMaybe (Int, [], []) (lookup ident fenv)

    insertFun :: Ident -> FnDefT -> FEnvT -> FEnvT
    insertFun ident def fenv = insert ident def fenv


--------------------------------------------------
----------------- EXPRESSIONS --------------------
--------------------------------------------------

    checkExpr :: Expr -> TM (Type)

    checkExpr (EVar ident) = do
        (env, fenv) <- ask
        lookupVar ident env
    
    checkExpr (ELitInt _) = return Int
    
    checkExpr (Neg e) = do
        t <- checkExpr e
        if t == Int
            then return Int
            else throwError ("Negating type " ++ show t)

    checkExpr (EMul e1 op e2) = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        if (t1 == Int && t2 == Int)
            then return Int
            else
                throwError ("Trying to do multiply/divide/modulo on " ++ (show t1) ++ "and " ++ (show t2))

    checkExpr (EAdd e1 op e2) = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        if (t1 == Int && t2 == Int)
            then return Int
            else
                throwError ("Trying to do add/subtract on " ++ (show t1) ++ "and " ++ (show t2))

    -- --string expr
    checkExpr (EString _) = return Str
    
    -- --bool expr
    checkExpr (ELitTrue) = return Bool
    
    checkExpr (ELitFalse) = return Bool
    
    checkExpr (Not e) = do
        t <- checkExpr e
        if t == Bool
            then return Bool
            else throwError ("Type error: !" ++ show t)
    
    checkExpr (ERel e1 op e2) = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        if (t1 == Int && t2 == Int)
            then return Bool
            else throwError ("Type error: rel operation on " ++ show t1 ++ " and " ++ show t2)

    checkExpr (EAnd e1 e2) = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        if (t1 == Bool && t2 == Bool)
            then return Bool
            else throwError ("Type error: " ++ show t1 ++ " && " ++ show t2)
    
    checkExpr (EOr e1 e2) = do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        if (t1 == Bool && t2 == Bool)
            then return Bool
            else throwError ("Type error: " ++ show t1 ++ " || " ++ show t2)

    --array expr
    checkExpr (ArrAcc a e) = do
        t <- checkExpr e
        if t /= Int
            then throwError ("Type error: trying to access array at index of type " ++ show t)
            else do
                (venv, fenv) <- ask
                t <- lookupVar a venv
                case t of
                    Arr x -> return x
                    _ -> throwError ("Type error: not an array but " ++ show t)

    -- --function expr

    --Iga: type FEnvT = Map Ident ([ArgType], [VEnvT])
    checkExpr (EApp ident rArgs) = do
        (venv, fenv) <- ask
        let (ret, fArgs, env) = lookupFun ident fenv
        validateArgs rArgs fArgs
        return ret

    --check if formal argument type is the same as actual one
    validateArgs :: [ExprOrRef] -> [ArgType] -> TM ()
    validateArgs [] [] = return ()
    validateArgs [] fArgs = throwError ("Number of arguments doesn't match")
    validateArgs rArgs [] = throwError ("Number of arguments doesn't match")
    validateArgs (ERefArg r:rs) (f:fs) = do
        case f of
            (ident, t, False) -> throwError ("Expected expression arg but got reference")
            (ident, t, True) -> do
                t' <- checkExpr (EVar r)
                if t /= t'
                    then throwError ("Argument types don't match")
                    else validateArgs rs fs

    validateArgs (EExpArg r:rs) (f:fs) = do
        case f of
            (ident, t, True) -> throwError("Expected reference but got expression argument")
            (ident, t, False) -> do
                t' <- checkExpr r
                if t /= t'
                    then throwError ("Argument types don't match")
                    else validateArgs rs fs


--------------------------------------------------
------------------ STATEMENTS --------------------
--------------------------------------------------

    checkRedecl :: Ident -> TM ()
    checkRedecl ident = do
        ((venv:venvs), _) <- ask
        if member ident venv
            then throwError ("Variable " ++ show ident ++ " already declared")
            else return ()

    checkStmtBlockHelper :: Block -> TM ()
    checkStmtBlockHelper (Block []) = return ()
    checkStmtBlockHelper (Block (b:bs)) = do
        (venv, fenv) <- checkStmt b
        -- return ()
        local (\_ -> (venv, fenv)) (checkStmtBlockHelper (Block bs))

    checkStmt :: Stmt -> TM (EnvT)
    
    --Iga: tu tak naprawdę trzeba wejść do wnętrza bloku i przepatrzeć
    checkStmt (BStmt (Block b)) = do
        (venv, fenv) <- ask
        local (\_ -> (Map.empty:venv, fenv)) (checkStmtBlockHelper (Block b))
        return (venv, fenv)

    checkStmt (Decl t (NoInit ident)) = do
        checkRedecl ident
        (venv, fenv) <- ask
        let venv' = insertVar ident t venv
        return (venv', fenv)

    checkStmt (Decl t (Init ident expr)) = do
        tExpr <- checkExpr expr
        if tExpr == t
            then checkStmt (Decl t (NoInit ident))
            else throwError ("Type error: initializing variable of type " ++ show t ++ " with expression of type " ++ show tExpr)

    checkStmt (Decl (Arr t) (ArrNoInit ident expr)) = do
        checkRedecl ident
        t1 <- checkExpr expr
        if t1 /= Int
            then throwError ("Type error: cannot declare array of size " ++ show t1)
            else do
                (venv, env) <- ask
                let venv' = insertVar ident (Arr t) venv
                return (venv', env)
    
    checkStmt (Decl (Arr t) (ArrInit ident expr [])) = do
        env <- checkStmt (Decl (Arr t) (ArrNoInit ident expr))
        return env


    --Iga: check that l.length is the size of array
    checkStmt (Decl (Arr t) (ArrInit ident expr (l:ls))) = do
        t1 <- checkExpr l
        if t /= t1
            then throwError ("Type error: initialization list contains " ++ show t1 ++ " instead of " ++ show t)
            else checkStmt (Decl (Arr t) (ArrInit ident expr ls))

    checkStmt (Ass ident e) = do
        (venv, fenv) <- ask
        t1 <- lookupVar ident venv
        t2 <- checkExpr e
        if t1 == t2
            then return (venv, fenv)
            else throwError ("Type error: assigning expression of type " ++ show t2 ++ " to variable of type " ++ show t2)
    
    checkStmt (ArrAss ident idx_e e) = do
        t <- checkExpr idx_e
        if t /= Int
            then throwError ("Type error: array index should be Int, not " ++ show t)
            else do
                (venv, fenv) <- ask
                t1 <- lookupVar ident venv
                case t1 of
                    Arr x -> do
                        t <- checkExpr e
                        if x == t
                            then return (venv, fenv)
                            else throwError ("Type error: trying to assign " ++ show t ++ " to array of type " ++ show x)
                    _ -> throwError ("Type error: trying to assing to array, but it's " ++ show t1)


    checkStmt (Ret e) = do
        t1 <- checkExpr e
        --Iga: sprawdzić, czy taki funkcji typ
        env <- ask
        return env

    checkStmt (VRet) = do
        --Iga: sprawdzić, czy taki funkcji typ
        env <- ask
        return env
    
    checkStmt (Cond e _ ) = do
        env <- ask
        t1 <- checkExpr e
        if t1 == Bool
            then return env
            else throwError ("While condition of type " ++ show t1)

    checkStmt (CondElse e _ _) = do
        env <- ask
        t1 <- checkExpr e
        if t1 == Bool
            then return env
            else throwError ("While condition of type " ++ show t1)

    checkStmt (While e _) = do
        env <- ask
        t1 <- checkExpr e
        if t1 == Bool
            then return env
            else throwError ("While condition of type " ++ show t1)


    checkStmt (For v start end (Block b)) = do
        env <- ask
        --Iga:sprawdzić, że zmienna v jest typu int
        t1 <- checkExpr start
        t2 <- checkExpr end
        if (t1 == Int && t2 == Int)
            then return env
            else throwError ("For loop from " ++ show t1 ++ " to " ++ show t2)
        --Iga:co z blokiem?

    checkStmt (Print e) = do
        env <- ask
        t <- checkExpr e
        return env

    checkStmt (SExp e) = do
        env <- ask
        t <- checkExpr e
        return env
    
    checkStmt (Break) = do
        env <- ask
        return env
    
    checkStmt (Cont) = do
        env <- ask
        return env

--------------------------------------------------
--------------------- RUN ------------------------
--------------------------------------------------

    checkProgram :: Program -> TM ()
    checkProgram (Program [f]) = do
        env <- checkFunction f
        return ()
    
    checkProgram (Program (f:fs)) = do 
                    env <- checkFunction f
                    local (\_ -> env) (checkProgram (Program fs))

    --make list of types of arguments
    prepArgTypes :: [ArgOrRef] -> [ArgType]
    prepArgTypes [] = []
    prepArgTypes (RefArg t ident:as) = (ident, t, True):prepArgTypes as
    prepArgTypes (Arg t ident:as) = (ident, t, False):prepArgTypes as

    addArgsToEnv :: [ArgType] -> VEnvT -> VEnvT
    addArgsToEnv [] env = env
    addArgsToEnv ((ident, t, isRef):args) env =
        let venv' = insert ident t env in
            addArgsToEnv args venv'

    checkFunction :: TopDef -> TM (EnvT)

    checkFunction (FnDef typ ident args block) = do
        (venv, fenv) <- ask
        let argTypes = prepArgTypes args
        let venv' = addArgsToEnv argTypes Map.empty
        local (\_ -> ((venv':venv), fenv)) (checkStmtBlockHelper block)
        return $ (venv, insertFun ident (typ, argTypes, venv) fenv)     
     

    checkProg prog = runExceptT $ runReaderT (checkProgram prog) ([], Map.empty)
