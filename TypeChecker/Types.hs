module TypeChecker.Types where

import Data.IORef
import Control.Monad.Reader
import qualified Data.Map as M
import qualified AbsSFL as SFL

type TypeVar = IORef (Maybe Type)
data Type = TypeVar TypeVar | TypeConstr String [Type]

data TypeScheme = Forall [TypeVar] Type

type Env = M.Map String TypeScheme
type Tc = ReaderT Env IO

data Exp
    = EVar String
    | EApp Exp Exp
    | ELam String Exp
--    | ELet String Exp Exp
    | ELet SFL.PatExp Exp Exp
    | EInt Integer
    | EBool Bool
    | EConstr String [Exp] deriving Show



tInt :: Type
tInt = TypeConstr "Int" []

tBool :: Type
tBool = TypeConstr "Bool" []