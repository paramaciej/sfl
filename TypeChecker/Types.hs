module TypeChecker.Types where

import Data.IORef
import Control.Monad.Reader
import Control.Monad.Except
import Exceptions.Types
import Interpreter.Show
import System.Console.ANSI
import qualified Data.Map as M
import qualified AbsSFL as SFL

data TypeVar = TV Int (IORef (Maybe Type))

instance Eq TypeVar where -- TODO a może jednak po prostu deriving Eq?
    (TV _ ioref) == (TV _ ioref') = ioref == ioref'

instance Show TypeVar where
    show (TV x _) = surroundSGR [SetItalicized True] $ "free" ++ show x

data Type = TypeVar TypeVar | TypeConstr String [Type]

data TypeScheme = Forall [TypeVar] Type

data Env = Env {
    maxIORef :: IORef Int,
    schemeMap :: M.Map String TypeScheme,
    typeConstrs :: M.Map String TCEntry}

data TCEntry = TCEntry { typeName :: String, typeTrans :: [Type] -> Tc Type}

type Tc = ExceptT TypeException (ReaderT Env IO)

data Exp
    = EVar String
    | EApp Exp Exp
    | ELam String Exp
    | ELet SFL.PatExp Exp Exp
    | ELetRec String Exp Exp
    | EInt Integer
    | EBool Bool
    | EIf Exp Exp Exp
    | EMatch Exp [(SFL.PatExp, Exp)]
    | EConstr String [Exp] deriving Show


tInt :: Type
tInt = TypeConstr "Int" []

tBool :: Type
tBool = TypeConstr "Bool" []
