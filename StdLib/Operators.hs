module StdLib.Operators where

import TypeChecker.Types
import TypeChecker.HMUtils
import Data.Map
import Control.Monad.Reader

tInt :: Type
tInt = TypeConstr "Int" []

ops :: MonadIO m => m Env
ops = do
    fr <- fresh
    return $ fromList [
        ("plus", Forall [] $ tApp tInt (tApp tInt tInt)),
        ("succ", Forall [] $ tApp tInt tInt),
        ("[]", Forall [fr] $ TypeConstr "list" [TypeVar fr]),
        ("cons", Forall [fr] $ tApp (TypeVar fr) (tApp (TypeConstr "list" [TypeVar fr]) (TypeConstr "list" [TypeVar fr])))
        ]

