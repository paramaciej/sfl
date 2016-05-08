module StdLib.Operators where

import TypeChecker.Types
import TypeChecker.HMUtils
import Interpreter.Types
import Data.Map
import Control.Monad.Reader


ops :: IO Env
ops = do
    fr <- fresh
    return $ fromList [
        ("plus", Forall [] $ mulTApp [tInt, tInt] tInt),
        ("succ", Forall [] $ mulTApp [tInt] tInt),
        ("[]", Forall [fr] $ TypeConstr "list" [TypeVar fr]),
        ("cons", Forall [fr] $ mulTApp [TypeVar fr, TypeConstr "list" [TypeVar fr]] (TypeConstr "list" [TypeVar fr])),
        ("_if", Forall [fr] $ mulTApp [tBool, TypeVar fr, TypeVar fr] (TypeVar fr))
        ]


eee :: (Map String Value)
eee = fromList [
        ("plus", curryV (\(VInt a, VInt b) -> VInt (a+b))),
        ("succ", VFun (\(VInt n) -> VInt (n + 1))),
        ("[]", VList []),
        ("cons", curryV (\(x, VList xs) -> VList (x:xs))),
        ("_if", VFun (\(VBool b) -> curryV (\(t, f) -> if b then t else f)))
        ]
