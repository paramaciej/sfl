module StdLib.Operators where

import TypeChecker.Types
import TypeChecker.HMUtils
import Interpreter.Types
import Data.Map
import Control.Monad.Reader


ops :: IO Env
ops = do
    fr <- fresh
    let binIntOp = Forall [] $ mulTApp [tInt, tInt] tInt
    let binBoolOp = Forall [] $ mulTApp [tBool, tBool] tBool
    let cmpOp = Forall [] $ mulTApp [tInt, tInt] tBool
    return $ fromList [
        ("add", binIntOp),
        ("sub", binIntOp),
        ("mul", binIntOp),
        ("div", binIntOp),
        ("mod", binIntOp),
        ("_and", binBoolOp),
        ("_or", binBoolOp),
        ("_not", Forall [] $ mulTApp [tBool] tBool),
        ("[]", Forall [fr] $ TypeConstr "list" [TypeVar fr]),
        ("cons", Forall [fr] $ mulTApp [TypeVar fr, TypeConstr "list" [TypeVar fr]] (TypeConstr "list" [TypeVar fr])),
        ("_if", Forall [fr] $ mulTApp [tBool, TypeVar fr, TypeVar fr] (TypeVar fr))
        ]


eee :: (Map String Value)
eee = fromList [
        ("add", curryV (\(VInt a, VInt b) -> return $ VInt (a + b))),
        ("sub", curryV (\(VInt a, VInt b) -> return $ VInt (a - b))),
        ("mul", curryV (\(VInt a, VInt b) -> return $ VInt (a * b))),
        ("div", curryV (\(VInt a, VInt b) -> return $ VInt (a `div` b))),
        ("mod", curryV (\(VInt a, VInt b) -> return $ VInt (a `mod` b))),

        ("_and", curryV (\(VBool a, VBool b) -> return $ VBool (a && b))),
        ("_or", curryV (\(VBool a, VBool b) -> return $ VBool (a || b))),
        ("_not", VFun (\(VBool a) -> return $ VBool (not a))),
        ("[]", VList []),
        ("cons", curryV (\(x, VList xs) -> return $ VList (x:xs))),
        ("_if", VFun (\(VBool b) -> return $ curryV (\(t, f) -> return $ if b then t else f)))
        ]
