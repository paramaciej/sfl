{-#LANGUAGE LambdaCase#-}

module Interpreter.Types where

import Data.Map
import Control.Monad.Reader
import Data.List

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
        VInt n -> show n
        VBool b -> show b
        VList list -> show list
        VTuple tuple -> "(" ++ (intercalate ", " $ Prelude.map show tuple) ++ ")"
        VFun _ -> "funkcja"
        VConstr name vals -> "V" ++ name ++ ": " ++ show vals
