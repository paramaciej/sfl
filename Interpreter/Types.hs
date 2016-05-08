{-#LANGUAGE LambdaCase#-}

module Interpreter.Types where

import Data.Map
import Control.Monad.Reader

data Value
    = VInt Integer
    | VBool Bool
    | VList [Value]
    | VTuple [Value]
    | VFun (Value -> Value)
    | VConstr String [Value]

type ValEnv = Map String Value
type VE = ReaderT ValEnv IO

curryV :: ((Value, Value) -> Value) -> Value
curryV fun = VFun (\x -> VFun (curry fun x))


instance Show Value where
    show = \case
        VInt n -> "VINT: " ++ show n
        VBool b -> "VBOOL: " ++ show b
        VList list -> "VLIST: " ++ show list
        VTuple tuple -> "VTuple" ++ show tuple
        VFun _ -> "VFUN"
        VConstr name vals -> "V" ++ name ++ ": " ++ show vals
