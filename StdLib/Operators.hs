module StdLib.Operators where

import TypeChecker.Types
import TypeChecker.HMUtils
import Data.Map
import Control.Monad.Reader

tInt :: Type
tInt = TypeConstr "Int" []

tBool :: Type
tBool = TypeConstr "Bool" []

ops :: MonadIO m => m Env
ops = do
    fr <- fresh
    return $ fromList [
        ("plus", Forall [] $ mulTApp [tInt, tInt] tInt),
        ("succ", Forall [] $ mulTApp [tInt] tInt),
        ("[]", Forall [fr] $ TypeConstr "list" [TypeVar fr]),
        ("cons", Forall [fr] $ mulTApp [TypeVar fr, TypeConstr "list" [TypeVar fr]] (TypeConstr "list" [TypeVar fr])),
        ("_if", Forall [fr] $ mulTApp [tBool, TypeVar fr, TypeVar fr] (TypeVar fr))
        ]

