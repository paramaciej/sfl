module StdLib.Operators where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map
import Exceptions.EvalErrors
import Interpreter.Types
import TypeChecker.Types
import TypeChecker.Utils


ops :: Tc Env
ops = do
    fr <- fresh
    let binIntOp = Forall [] $ mulTApp [tInt, tInt] tInt
    let binBoolOp = Forall [] $ mulTApp [tBool, tBool] tBool
    let cmpOp = Forall [] $ mulTApp [tInt, tInt] tBool


    let o =  fromList [
            ("add", binIntOp),
            ("sub", binIntOp),
            ("mul", binIntOp),
            ("div", binIntOp),
            ("mod", binIntOp),

            ("_and", binBoolOp),
            ("_or", binBoolOp),
            ("_not", Forall [] $ mulTApp [tBool] tBool),

            ("_lt", cmpOp),
            ("_lte", cmpOp),
            ("_gt", cmpOp),
            ("_gte", cmpOp),
            ("_eq", cmpOp),
            ("_neq", cmpOp),

            ("[]", Forall [fr] $ TypeConstr "list" [TypeVar fr]),
            ("cons", Forall [fr] $ mulTApp [TypeVar fr, TypeConstr "list" [TypeVar fr]] (TypeConstr "list" [TypeVar fr])),
            ("_infer_if", Forall [fr] $ mulTApp [tBool, TypeVar fr, TypeVar fr] (TypeVar fr))
            ]
    let td = fromList [
            ("Int", 0),
            ("Bool", 0)
            ]
    local (\(Env m _ tc _) -> Env m o tc td) ask

eee :: (Map String Value)
eee = fromList [
    ("add", funIntIntInt (+)),
    ("sub", funIntIntInt (-)),
    ("mul", funIntIntInt (*)),
    ("div", funFailOnZero div),
    ("mod", funFailOnZero mod),

    ("_and", curryV (\(VBool a, VBool b) -> return $ VBool (a && b))),
    ("_or", curryV (\(VBool a, VBool b) -> return $ VBool (a || b))),
    ("_not", VFun (\(VBool a) -> return $ VBool (not a))),

    ("_lt", funIntIntBool (<)),
    ("_lte", funIntIntBool (<=)),
    ("_gt", funIntIntBool (>)),
    ("_gte", funIntIntBool (>=)),
    ("_eq", funIntIntBool (==)),
    ("_neq", funIntIntBool (/=)),

    ("[]", VList []),
    ("cons", curryV (\(x, VList xs) -> return $ VList (x:xs)))
    ]
  where
    funIntIntInt fun = curryV (\(VInt a, VInt b) -> return $ VInt (fun a b))
    funIntIntBool fun = curryV (\(VInt a, VInt b) -> return $ VBool (fun a b))

    funFailOnZero fun = curryV (\(VInt a, VInt b) -> if b == 0
        then throwError ZeroDivisionError
        else return $ VInt (fun a b))



