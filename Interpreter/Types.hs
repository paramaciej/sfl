{-# LANGUAGE LambdaCase #-}

module Interpreter.Types where

import Data.List
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import TypeChecker.Types
import TypeChecker.Show
import System.Console.ANSI

data ProgramEnv = PrEnv {types :: Env, values :: ValEnv}
type PrSt = ExceptT String (StateT ProgramEnv IO)


data Value
    = VInt Integer
    | VBool Bool
    | VList [Value]
    | VTuple [Value]
    | VFun (Value -> VE Value)
    | VConstr String [Value]

type ValEnv = Map String Value
type VE = ReaderT ValEnv IO

curryV :: ((Value, Value) -> VE Value) -> Value
curryV fun = VFun (return . VFun . curry fun)


instance Show Value where
    show = \case
        VInt n -> surroundSGR [SetColor Foreground Dull Magenta] $ show n
        VBool b -> surroundSGR [SetColor Foreground Dull Cyan] $ show b
        VList list -> show list
        VTuple tuple -> "(" ++ intercalate ", " (Prelude.map show tuple) ++ ")"
        VFun _ -> "function"
        VConstr name vals -> "V" ++ surroundSGR [SetColor Foreground Dull Green] name ++ ": " ++ show vals
