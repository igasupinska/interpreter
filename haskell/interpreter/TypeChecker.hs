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
    type EnvT = ([VEnvT], VEnvT, FEnvT)

    --keeps type of argument and info whether its reference or not
    --(typ, isRef)
    type ArgType = (Ident, Type, Bool)


    lookupVar :: Ident -> [VEnvT] -> TM (Type)
    lookupVar id@(Ident ident) [] = do
        (_, gvenv, _) <- ask
        if member id gvenv
            then return $ gvenv ! id
            else throwError ("Variable " ++ ident ++ " not declared.")

    lookupVar ident (e:es) = do
        if member ident e
            then return $ e ! ident
            else lookupVar ident es

    insertVar :: Ident -> Type -> TM ([VEnvT])
    insertVar id@(Ident ident) typ = do
        ((v:vs), _, _) <- ask
        if member id v
            then throwError ("Variable " ++ ident ++ " already declared.")
            else do
                let v' = insert id typ v
                return $ (v':vs)

    insertGlobalVar :: Ident -> Type -> TM(VEnvT)
    insertGlobalVar id@(Ident ident) typ = do
        (_, gvenv, _) <- ask
        if member id gvenv
            then throwError ("Global variable " ++ ident ++ " already declared.")
            else do
                let gvenv' = insert id typ gvenv
                return gvenv'

    lookupFun :: Ident -> TM (FnDefT)
    lookupFun id@(Ident ident) = do
        (_, _, fenv) <- ask
        if member id fenv
            then return $ fenv ! id
            else throwError ("Function " ++ ident ++ " not defined.")

    insertFun :: Ident -> FnDefT -> TM (FEnvT)
    insertFun id@(Ident ident) def = do
        (_, _, fenv) <- ask
        if member id fenv
            then throwError ("Function " ++ ident ++ " already defined.")
            else do
                let fenv' = insert id def fenv
                return fenv'


--------------------------------------------------
----------------- EXPRESSIONS --------------------
--------------------------------------------------

    correctTypes :: Type -> [Expr] -> TM ()
    correctTypes expT [] = return ()

    correctTypes expT (e:es) = do
        correctType expT e
        correctTypes expT es
        return ()

    correctType :: Type -> Expr -> TM ()
    correctType expT e = do
        t <- checkExpr e
        if t == expT
            then return ()
            else throwError ("Expected " ++ show expT ++ " but got " ++ show t)

    checkExpr :: Expr -> TM (Type)

    checkExpr (EVar ident) = do
        (env, gvenv, fenv) <- ask
        lookupVar ident env
    
    checkExpr (ELitInt _) = return Int
    
    checkExpr (Neg e) = do
        correctType Int e
        return Int
        `catchError` \err -> throwError ("Type error: in " ++ show (Neg e)  ++ " operation. "++ err)

    checkExpr (EMul e1 op e2) = do
        correctType Int e1
        correctType Int e2
        return Int
        `catchError` \err -> throwError ("Type error: in " ++ show op  ++ " operation. "++ err)

    checkExpr (EAdd e1 op e2) = do
        correctType Int e1
        correctType Int e2
        return Int
        `catchError` \err -> throwError ("Type error: in" ++ (show op) ++ "operation. " ++ err)

    -- --string expr
    checkExpr (EString _) = return Str
    
    -- --bool expr
    checkExpr (ELitTrue) = return Bool
    
    checkExpr (ELitFalse) = return Bool
    
    checkExpr (Not e) = do
        correctType Bool e
        return Bool
        `catchError` \err -> throwError ("Type error: in " ++ show (Not e) ++ " operation. " ++ err)
    
    checkExpr (ERel e1 op e2) = do
        correctType Int e1
        correctType Int e2
        return Bool
        `catchError` \err -> throwError ("Type error: in" ++ (show op) ++ "operation. " ++ err)

    checkExpr (EAnd e1 e2) = do
        correctType Bool e1
        correctType Bool e2
        return Bool
        `catchError` \err -> throwError ("Type error: in && operation. " ++ err)

    checkExpr (EOr e1 e2) = do
        correctType Bool e1
        correctType Bool e2
        return Bool
        `catchError` \err -> throwError ("Type error: in || operation. " ++ err)

    --array expr
    checkExpr (ArrAcc a e) = do
        correctType Int e
        (venv, gvenv, fenv) <- ask
        t <- lookupVar a venv
        case t of
            Arr x -> return x
            _     -> throwError ("Not an array but " ++ show t)
        `catchError` \err -> throwError ("Type error: in array access. " ++ err)

    -- --function expr

    --Iga:upiekszyc
    --Iga: type FEnvT = Map Ident ([ArgType], [VEnvT])
    checkExpr (EApp ident rArgs) = do
        (ret, fArgs, env) <- lookupFun ident
        validateArgs rArgs fArgs
        return ret

    --Iga:upiekszyc
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
    --Iga:upiekszyc
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
    
    --Iga:upiekszyc
    checkStmtBlockHelper :: Block -> TM ()
    checkStmtBlockHelper (Block []) = return ()
    checkStmtBlockHelper (Block (b:bs)) = do
        (venv, gvenv, fenv) <- checkStmt b
        -- return ()
        local (\_ -> (venv, gvenv, fenv)) (checkStmtBlockHelper (Block bs))

    checkStmt :: Stmt -> TM (EnvT)
    
    --Iga: tu tak naprawdę trzeba wejść do wnętrza bloku i przepatrzeć
    checkStmt (BStmt (Block b)) = do
        (venv, gvenv, fenv) <- ask
        local (\_ -> (Map.empty:venv, gvenv, fenv)) (checkStmtBlockHelper (Block b))
        return (venv, gvenv, fenv)

    checkStmt (Decl t (NoInit ident)) = do
        (venv, gvenv, fenv) <- ask
        venv' <- insertVar ident t
        return (venv', gvenv, fenv)

    checkStmt (Decl t (Init ident expr)) = do
        correctType t expr
        checkStmt (Decl t (NoInit ident))
        `catchError` \err -> throwError ("Type error: variable declaration. " ++ err)

    checkStmt (Decl (Arr t) (ArrNoInit ident expr)) = do
        correctType Int expr
        (venv, gvenv, fenv) <- ask
        venv' <- insertVar ident (Arr t)
        return (venv', gvenv, fenv)
        `catchError` \err -> throwError ("Type error: in array size expression. " ++ err)
    
    checkStmt (Decl (Arr t) (ArrInit ident expr l)) = do
        env <- checkStmt (Decl (Arr t) (ArrNoInit ident expr))
        correctTypes t l
        return env
        `catchError` \err -> throwError ("Type error: list initialization. " ++ err)

    checkStmt (Ass ident e) = do
        (venv, gvenv, fenv) <- ask
        t1 <- lookupVar ident venv
        correctType t1 e
        return (venv, gvenv, fenv)
        `catchError` \err -> throwError ("Type error: in assignment. " ++ err)
    
    checkStmt (ArrAss id@(Ident ident) idx_e e) = do
        correctType Int idx_e
        (venv, gvenv, fenv) <- ask
        t1 <- lookupVar id venv
        case t1 of
            Arr x -> do
                correctType x e
                return (venv, gvenv, fenv)
            _ -> throwError (ident ++ " is not an array.")
        `catchError` \err -> throwError ("Type error: array assignment. " ++ err)

    checkStmt (Ret e) = do
        t1 <- checkExpr e
        --Iga: sprawdzić, czy taki funkcji typ
        env <- ask
        return env

    checkStmt (VRet) = do
        --Iga: sprawdzić, czy taki funkcji typ
        env <- ask
        return env
    
    checkStmt (Cond e _) = do
        env <- ask
        correctType Bool e
        return env
        `catchError` \err -> throwError ("Type error: if condition. " ++ err)

    checkStmt (CondElse e _ _) = do
        env <- ask
        correctType Bool e
        return env
        `catchError` \err -> throwError ("Type error: if-else condition. " ++ err)

    checkStmt (While e _) = do
        env <- ask
        correctType Bool e
        return env
        `catchError` \err -> throwError ("Type error: while condition. " ++ err)

    checkStmt (For v start end (Block b)) = do
        env <- ask
        --Iga:sprawdzić, że zmienna v jest typu int
        correctType Int start
        correctType Int end
        correctType Int (EVar v)
        return env
        `catchError` \err -> throwError ("Type error: for loop. " ++ err)
        --Iga:co z blokiem?


    --Iga: co z tym poniżej
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
    checkProgram (Program []) = return ()
    
    checkProgram (Program (f:fs)) = do
                    case f of
                        (FnDef _ _ _ _) -> do
                                env <- checkFunction f
                                local (\_ -> env) (checkProgram (Program fs))
                        (GlobalVar t i) -> do
                                env <- checkGlobal f
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

    checkGlobal :: TopDef -> TM (EnvT)
    checkGlobal (GlobalVar typ item) = do
        (venv, gvenv, fenv) <- ask
        case item of
            (NoInit id@(Ident ident)) -> do
                gvenv' <- insertGlobalVar id typ
                return (venv, gvenv', fenv)
            (Init id@(Ident ident) _) -> do
                gvenv' <- insertGlobalVar id typ
                return (venv, gvenv', fenv)
            (ArrNoInit id@(Ident ident) _) -> do
                gvenv' <- insertGlobalVar id (Arr typ)
                return (venv, gvenv', fenv)
            (ArrInit id@(Ident ident) _ _) -> do
                gvenv' <- insertGlobalVar id (Arr typ)
                return (venv, gvenv', fenv)

    checkFunction :: TopDef -> TM (EnvT)

    checkFunction (FnDef typ ident args block) = do
        (venv, gvenv, fenv) <- ask
        let argTypes = prepArgTypes args
        let venv' = addArgsToEnv argTypes Map.empty
        fenv' <- insertFun ident (typ, argTypes, venv)
        local (\_ -> ((venv':venv), gvenv, fenv')) (checkStmtBlockHelper block)
        return $ (venv, gvenv, fenv')
     

    checkProg prog = runExceptT $ runReaderT (checkProgram prog) ([], Map.empty, Map.empty)
