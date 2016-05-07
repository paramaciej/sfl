module TypeChecker.Types where

import Data.IORef
import Control.Monad.Reader
import qualified Data.Map as M

type TypeVar = IORef (Maybe Type)
data Type = TypeVar TypeVar | TypeConstr String [Type]

data TypeScheme = Forall [TypeVar] Type

type Env = M.Map String TypeScheme
type Tc = ReaderT Env IO

data Exp
    = EVar String
    | EApp Exp Exp
    | ELam String Exp
    | ELet String Exp Exp
    | EInt Integer
    | EConstr String [Exp] deriving Show