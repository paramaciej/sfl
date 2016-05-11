module StdLib.Operators where

import TypeChecker.Types
import TypeChecker.HMUtils
import Interpreter.Types
import Data.Map


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

eee :: (Map String Value)
eee = fromList [
    ("add", funIntIntInt (+)),
    ("sub", funIntIntInt (-)),
    ("mul", funIntIntInt (*)),
    ("div", funIntIntInt div),
    ("mod", funIntIntInt mod),

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



