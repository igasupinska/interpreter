module Helper where
    import AbsGramm

    import Control.Monad.Reader
    import Control.Monad.State.Lazy
    import Control.Monad.Except
    import Control.Exception
    import Data.Map as Map
    import Data.Maybe
    import Prelude hiding (lookup)
    
    import Types


    getNewLoc :: Store -> Store
    getNewLoc (store, lastLoc) = (store, lastLoc + 1)

    lookupVar :: Ident -> MM (Loc)
    lookupVar ident = do
        env <- ask
        if member ident (vEnv env)
            then return $ (vEnv env) ! ident
            else return $ (gEnv env) ! ident

    insertVar :: Ident -> Loc -> VEnv -> VEnv
    insertVar name loc env = insert name loc env

    insertGlobalVar :: Ident -> MM (GEnv)
    insertGlobalVar ident = do
        (s, loc) <- get
        modify (getNewLoc)
        env <- ask
        return $ insert ident loc (gEnv env)


    lookupFun :: Ident -> FEnv -> (TopDef, GEnv)
    lookupFun ident fenv = fenv ! ident

    insertFun :: Ident -> (TopDef, GEnv) -> FEnv -> FEnv
    insertFun ident def fenv = insert ident def fenv

    lookupStore :: Loc -> Store -> StoredVal
    lookupStore loc (store, _) = store ! loc

    insertStore :: Loc -> StoredVal -> Store -> Store
    insertStore loc val (store, lastLoc) = (insert loc val store, lastLoc)


    storeArray :: Integer -> [StoredVal] -> MM (StoredVal)
    storeArray size vals = do
        let idx = [0..size]
        let array = zip idx vals
        return $ SArr $ fromList array

    validateIndex :: Map Integer StoredVal -> Integer -> MM()
    validateIndex arr idx = do
        if fromInteger idx > size arr
        then throwError OutOfBound
            else
                if idx < 0
                    then throwError NegIndex
                    else return()