{-# LANGUAGE LambdaCase #-}

module Interpreter.Types where

import Data.List
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Exceptions.EvalErrors
import TypeChecker.Types
import System.Console.ANSI
import Interpreter.Show

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
type VE = ExceptT EvalError (ReaderT ValEnv IO)

curryV :: ((Value, Value) -> VE Value) -> Value
curryV fun = VFun (return . VFun . curry fun)


instance Show Value where
    show = \case
        VInt n -> surroundSGR [SetColor Foreground Dull Magenta] $ show n
        VBool b -> surroundSGR [SetColor Foreground Dull Cyan] $ show b
        VList list -> show list
        VTuple tuple -> let blue = surroundSGR [SetColor Foreground Dull Blue] in
            blue "(" ++ intercalate (blue ", ") (Prelude.map show tuple) ++ blue ")"
        VFun _ -> surroundSGR [SetColor Foreground Dull Yellow] "function"
        VConstr name vals -> surroundSGR [SetColor Foreground Dull Green] name ++
            concatMap (\val -> " " ++ showArg val) vals
          where
            showArg v = case v of
                VConstr _ (_:_) -> "(" ++ show v ++ ")"
                _ -> show v
