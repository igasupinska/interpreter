 module Types where
    import AbsGramm

    import Control.Monad.Reader
    import Control.Monad.State.Lazy
    import Control.Monad.Except
    import Control.Exception
    import Data.Map as Map
    import Data.Maybe
    import Prelude hiding (lookup)
    

    data StoredVal = SInt Integer | SStr String | SBool Bool | SArr (Map Integer StoredVal)

    type Loc = Integer
    
    --keeps mappings from location into values and last used location
    type Store = (Map Loc StoredVal, Loc)

    --variable environment
    type VEnv = Map Ident Loc

    --global variable environment
    type GEnv = Map Ident Loc
    
    --function environment
    type FEnv = Map Ident (TopDef, GEnv)

    data Env = Env
        { vEnv :: VEnv
        , gEnv :: GEnv
        , fEnv :: FEnv
        }

    initialEnv = Env {vEnv = Map.empty, gEnv = Map.empty, fEnv = Map.empty}
    
    data Flag = FReturn | FBreak | FContinue | FNothing

    data MyException = DivZero | ModZero | OutOfBound | NegIndex | InvalidSize

    -- Converts MyException to a readable message.
    instance Show MyException where
      show DivZero = "Trying to divide by 0"
      show ModZero = "Modulo of 0"
      show OutOfBound = "Index out of bound"
      show NegIndex = "Index negative"
      show InvalidSize = "Array of invalid size"

    --my monad
    type MM = ReaderT Env (StateT Store (ExceptT MyException IO))

    --Iga: data Type = Int | Str | Bool | Void | Arr Type
    --Iga: co z arr i void?
    getDefaultExpr :: Type -> Expr
    getDefaultExpr Int = ELitInt 0
    getDefaultExpr Str = EString []
    getDefaultExpr Bool = ELitFalse
    getDefaultExpr arr = getDefaultExpr $ getArrType arr


    isArray :: Type -> Bool
    isArray ArrInt = True
    isArray ArrStr = True
    isArray ArrBool = True
    isArray a = False

    getArrType :: Type -> Type
    getArrType ArrInt = Int
    getArrType ArrStr = Str
    getArrType ArrBool = Bool