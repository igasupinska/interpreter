module TypeChecker where
    import AbsGramm

    import Types

    import Control.Monad.Reader
    import Control.Monad.State.Lazy
    import Control.Monad.Except
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

    --global variable types environment
    type GEnvT = Map Ident Type    
    
    --function definition type
    data FnDefT = FnDefT
        { retT  :: Type         --return type
        , argsT :: [ArgType]    --types of arguments
        , venvT :: [VEnvT]      --visible environments
        }

    --function types environment
    type FEnvT = Map Ident FnDefT

    --note that environment keeps list of visible environments,
    --starting from the most nested one and going outwards
    data EnvT = EnvT
        { vEnvT :: [VEnvT]
        , gEnvT :: GEnvT        
        , fEnvT :: FEnvT
        }

    initialEnvT = EnvT {vEnvT = [], gEnvT = Map.empty, fEnvT = Map.empty}

    data ArgType = ArgType
        { ident :: Ident
        , typ :: Type
        , isRef :: Bool
        }

    data FlagT = FBreakT | FContinueT | FNothingT

    lookupVar :: Ident -> [VEnvT] -> TM (Type)
    lookupVar id@(Ident ident) [] = do
        env <- ask
        if member id (gEnvT env)
            then return $ (gEnvT env) ! id
            else throwError ("Variable " ++ ident ++ " not declared.")

    lookupVar ident (e:es) = do
        if member ident e
            then return $ e ! ident
            else lookupVar ident es

    insertVar :: Ident -> Type -> TM ([VEnvT])
    insertVar id@(Ident ident) typ = do
        env <- ask
        let (v:vs) = vEnvT env
        if member id v
            then throwError ("Variable " ++ ident ++ " already declared.")
            else do
                let v' = insert id typ v
                return $ (v':vs)

    insertGlobalVar :: Ident -> Type -> TM (GEnvT)
    insertGlobalVar id@(Ident ident) typ = do
        env <- ask
        if member id (gEnvT env)
            then throwError ("Global variable " ++ ident ++ " already declared.")
            else do
                let gvenv' = insert id typ (gEnvT env)
                return gvenv'

    lookupFun :: Ident -> TM (FnDefT)
    lookupFun id@(Ident ident) = do
        env <- ask
        if member id (fEnvT env)
            then return $ (fEnvT env) ! id
            else throwError ("Function " ++ ident ++ " not defined.")

    insertFun :: Ident -> FnDefT -> TM (FEnvT)
    insertFun id@(Ident ident) def = do
        env <- ask
        if member id (fEnvT env)
            then throwError ("Function " ++ ident ++ " already defined.")
            else do
                let fenv' = insert id def (fEnvT env)
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
        env <- ask
        lookupVar ident (vEnvT env)
    
    --int expr
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

    --string expr
    checkExpr (EString _) = return Str
    
    --bool expr
    checkExpr (ELitTrue) = return Bool
    
    checkExpr (ELitFalse) = return Bool
    
    checkExpr (Not e) = do
        correctType Bool e
        return Bool
        `catchError` \err -> throwError ("In " ++ show (Not e) ++ " operation. " ++ err)
    
    checkExpr (ERel e1 op e2) = do
        correctType Int e1
        correctType Int e2
        return Bool
        `catchError` \err -> throwError ("In" ++ (show op) ++ "operation. " ++ err)

    checkExpr (EAnd e1 e2) = do
        correctType Bool e1
        correctType Bool e2
        return Bool
        `catchError` \err -> throwError ("In && operation. " ++ err)

    checkExpr (EOr e1 e2) = do
        correctType Bool e1
        correctType Bool e2
        return Bool
        `catchError` \err -> throwError ("In || operation. " ++ err)

    --array expr
    checkExpr (ArrAcc (ArrItem a e)) = do
        correctType Int e
        env <- ask
        t <- lookupVar a (vEnvT env)
        if isArray t
            then return $ getArrType t
            else throwError ("Not an array but " ++ show t)
        `catchError` \err -> throwError ("In array access. " ++ err)

    --function expr
    checkExpr (EApp id@(Ident ident) rArgs) = do
        funDef <- lookupFun id
        let fArgs = argsT funDef
        if length fArgs /= length rArgs
            then throwError ("number of arguments doesn't match.")
            else do
                validateArgs rArgs (argsT funDef)
                return $ retT funDef
        `catchError` \err -> throwError ("In function " ++ ident ++ ": " ++ err)

    --helper

    --checks if formal argument types are the same as actual ones
    validateArgs :: [ExprOrRef] -> [ArgType] -> TM ()
    validateArgs [] [] = return ()    
    validateArgs (ERefArg r:rs) (f:fs) = do
        if (isRef f)
            then do
                t' <- checkExpr (EVar r)
                if t' /= (typ f)
                    then throwError ("argument types don't match.")
                    else validateArgs rs fs
            else throwError ("expected expression argument but got reference.")
    validateArgs (EExpArg r:rs) (f:fs) = do
        if (isRef f)
            then throwError("expected reference but got expression argument.")
            else do
                t' <- checkExpr r
                if t' /= (typ f)
                    then throwError ("argument types don't match.")
                    else validateArgs rs fs


--------------------------------------------------
------------------ STATEMENTS --------------------
--------------------------------------------------
    
    checkStmtBlockHelper :: Block -> [Maybe Type] -> FlagT -> TM (Maybe Type, FlagT)
    checkStmtBlockHelper (Block []) (r:rs) flag = do
        if all (\x -> x == r || isNothing x) rs
            then return (r, flag)
            else throwError ("different return types.")
    checkStmtBlockHelper (Block (b:bs)) ret flag = do
        (env, r, f) <- checkStmt b
        case f of
            FNothingT -> local (\_ -> env) (checkStmtBlockHelper (Block bs) (r:ret) flag)
            _ -> local (\_ -> env) (checkStmtBlockHelper (Block bs) (r:ret) f)

    checkStmt :: Stmt -> TM (EnvT, Maybe Type, FlagT)

    checkStmt (BStmt (Block b)) = do
        env <- ask
        (ret, flag) <- local (\_ -> env {vEnvT = (Map.empty: vEnvT env)}) (checkStmtBlockHelper (Block b) [] FNothingT)
        return (env, ret, flag)

    checkStmt (Decl t (NoInit ident)) = do
        env <- ask
        venv' <- insertVar ident t
        return (env {vEnvT = venv'}, Nothing, FNothingT)

    checkStmt (Decl t (Init ident expr)) = do
        correctType t expr
        checkStmt (Decl t (NoInit ident))
        `catchError` \err -> throwError ("In variable declaration. " ++ err)

    checkStmt (Decl t (ArrNoInit ident expr)) = do
        correctType Int expr
        env <- ask
        venv' <- insertVar ident t
        return (env {vEnvT = venv'}, Nothing, FNothingT)
        `catchError` \err -> throwError ("In array size expression. " ++ err)
    
    checkStmt (Decl t (ArrInit ident expr l)) = do
        (env, _, _) <- checkStmt (Decl t (ArrNoInit ident expr))
        correctTypes (getArrType t) l
        return (env, Nothing, FNothingT)
        `catchError` \err -> throwError ("In list initialization. " ++ err)

    checkStmt (Ass ident e) = do
        env <- ask
        t1 <- lookupVar ident (vEnvT env)
        correctType t1 e
        return (env, Nothing, FNothingT)
        `catchError` \err -> throwError ("In assignment. " ++ err)
    
    checkStmt (ArrAss (ArrItem id@(Ident ident) idx_e) e) = do
        correctType Int idx_e
        env <- ask
        t1 <- lookupVar id (vEnvT env)
        if isArray t1
            then do
                correctType (getArrType t1) e
                return (env, Nothing, FNothingT)
            else throwError (ident ++ " is not an array.")
        `catchError` \err -> throwError ("In array assignment. " ++ err)

    checkStmt (Ret e) = do
        t <- checkExpr e
        env <- ask
        return (env, Just t, FNothingT)

    checkStmt (VRet) = do
        env <- ask
        return (env, Just Void, FNothingT)
    
    checkStmt (Cond e b) = do
        env <- ask
        correctType Bool e
        (_, ret, flag) <- checkStmt $ BStmt b
        return (env, ret, flag)
        `catchError` \err -> throwError ("In if. " ++ err)

    --Iga: tu wrócić
    checkStmt (CondElse e if_b else_b) = do
        env <- ask
        correctType Bool e
        (_, ret1, flag1) <- checkStmt $ BStmt if_b
        (_, ret2, flag2) <- checkStmt $ BStmt else_b
        let flag = chooseFlag flag1 flag2
        if ret1 == ret2 || isNothing ret1 || isNothing ret2
            then if isNothing ret2
                    then return (env, ret1, flag)
                    else return (env, ret2, flag)
        else throwError ("Return statements of different types: " ++ show ret1 ++ " and " ++ show ret2)
        `catchError` \err -> throwError ("In if-else. " ++ err)

    checkStmt (While e b) = do
        env <- ask
        correctType Bool e
        (_, ret, flag) <- checkStmt $ BStmt b
        return (env, ret, FNothingT)
        `catchError` \err -> throwError ("In while loop. " ++ err)

    checkStmt (For v start end b) = do
        env <- ask
        correctType Int start
        correctType Int end
        correctType Int (EVar v)
        (_, ret, flag) <- checkStmt $ BStmt b
        return (env, ret, flag)
        `catchError` \err -> throwError ("In for loop. " ++ err)

    checkStmt (Print e) = do
        env <- ask
        t <- checkExpr e
        if isArray t || t == Void
            then throwError ("In print: " ++ show t ++ " not printable.")
            else return (env, Nothing, FNothingT)

    checkStmt (SExp e) = do
        env <- ask
        t <- checkExpr e
        return (env, Nothing, FNothingT)
    
    checkStmt (Break) = do
        env <- ask
        return (env, Nothing, FBreakT)
    
    checkStmt (Cont) = do
        env <- ask
        return (env, Nothing, FContinueT)

    --helper
    chooseFlag :: FlagT -> FlagT -> FlagT
    chooseFlag FNothingT flag = flag
    chooseFlag flag FNothingT = flag
    chooseFlag f1 f2 = f1
--------------------------------------------------
--------------------- RUN ------------------------
--------------------------------------------------

    checkProgram :: Program -> TM ()
    checkProgram (Program []) = do
        mainDef <- lookupFun (Ident "main")
        if retT mainDef /= Int
            then throwError ("Main function must return int")
            else if length (argsT mainDef) /= 0
                then throwError ("Main takes no arguments")
                else return ()
    
    checkProgram (Program (f:fs)) = do
                    case f of
                        (FnDef _ _ _ _) -> do
                                env <- checkFunction f
                                local (\_ -> env) (checkProgram (Program fs))
                        (GlobalVar t i) -> do
                                env <- checkGlobal f
                                local (\_ -> env) (checkProgram (Program fs))


    checkFunction :: TopDef -> TM (EnvT)

    checkFunction (FnDef typ id@(Ident ident) args block) = do
        env <- ask
        let argTypes = prepArgTypes args
        let venv' = addArgsToEnv argTypes Map.empty
        fenv' <- insertFun id (FnDefT {retT = typ, argsT = argTypes, venvT = (vEnvT env)})
        (ret, flag) <- local (\_ -> env {vEnvT = (venv':vEnvT env), fEnvT = fenv'}) (checkStmtBlockHelper block [] FNothingT)
        checkFlag flag
        checkReturn ret typ
        return env {fEnvT = fenv'}
        `catchError` \err -> throwError ("Type error in function " ++ ident ++ ". " ++ err)

    checkReturn :: Maybe Type -> Type -> TM ()
    checkReturn (Just retT) funT = do
        if retT == funT
            then return ()
            else throwError ("Wrong type of return. Expecting " ++ show funT ++ " but got " ++ show retT)
            
    checkReturn Nothing funT = do
        if Void == funT
            then return ()
            else throwError ("No return statement in non-void function.")

    checkFlag :: FlagT -> TM ()
    checkFlag flag = do
        case flag of
            FNothingT -> return ()
            FBreakT -> throwError ("Break statement not within loop.")
            FContinueT -> throwError ("Continue statement not within loop.")

    --make list of types of arguments
    prepArgTypes :: [ArgOrRef] -> [ArgType]
    prepArgTypes [] = []
    prepArgTypes (RefArg t ident:as) = ArgType {ident = ident, typ = t, isRef = True}:prepArgTypes as
    prepArgTypes (Arg t ident:as) = ArgType {ident = ident, typ = t, isRef = False}:prepArgTypes as

    addArgsToEnv :: [ArgType] -> VEnvT -> VEnvT
    addArgsToEnv [] env = env
    addArgsToEnv (a:args) env =
        let venv' = insert (ident a) (typ a) env in
            addArgsToEnv args venv'

    checkGlobal :: TopDef -> TM (EnvT)
    checkGlobal (GlobalVar typ item) = do
        env <- ask
        case item of
            (NoInit id@(Ident ident)) -> do
                gvenv' <- insertGlobalVar id typ
                return env {gEnvT = gvenv'}
            (Init id@(Ident ident) _) -> do
                gvenv' <- insertGlobalVar id typ
                return env {gEnvT = gvenv'}
            (ArrNoInit id@(Ident ident) _) -> do
                gvenv' <- insertGlobalVar id typ
                return env {gEnvT = gvenv'}
            (ArrInit id@(Ident ident) _ _) -> do
                gvenv' <- insertGlobalVar id typ
                return env {gEnvT = gvenv'}



    checkProg prog = runExceptT $ runReaderT (checkProgram prog) initialEnvT