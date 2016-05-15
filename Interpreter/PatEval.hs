{-# LANGUAGE LambdaCase #-}
module Interpreter.PatEval where

import Interpreter.Types
import AbsSFL as SFL
import Data.Map
import Control.Monad.Reader

patEval :: SFL.PatExp -> Value -> VE (ValEnv -> ValEnv)
patEval patExp val = case patExp of
    PETuple pe1 pe2 -> case val of
        VTuple [v1, v2] -> do
            ev1 <- patEval pe1 v1
            ev2 <- patEval pe2 v2
            return (ev1 . ev2)
        _ -> error $ "wrong value for tuple: " ++ show val -- FIXME
    PECons pe1 pe2 -> case val of
        VList (v:vs) -> do
            ev1 <- patEval pe1 v
            ev2 <- patEval pe2 (VList vs)
            return (ev1 . ev2)
        VList [] -> error $ "matching with empty list!" -- FIXME
        _ -> error $ "wrong value for list: " ++ show val -- FIXME
    PEPat (PatVar (Ident name)) -> return $ insert name val
    PEPat (PatTConstr (UIdent name) pats) -> case val of
        VConstr n vs -> if n == name
            then do
                evs <- mapM (\(p, a) -> patEval p a) (zip pats vs)
                let composed = comp evs where
                    comp [] = id
                    comp (h:t) = h . (comp t)
                return composed
            else error "constructor names mismatch!"
        _ -> error $ "wrong value of constructor: " ++ show val
    PEPat (PatWild) -> return id
    PEPat (PatTrue) -> return id
    PEPat (PatFalse) -> return id
    PEPat (PatInt _) -> return id




patEquals :: SFL.PatExp -> Value -> Bool
patEquals patExp val = case patExp of
    PETuple pe1 pe2 -> case val of
        VTuple [v1, v2] -> patEquals pe1 v1 && patEquals pe2 v2
    PECons pe1 pe2 -> case val of
        VList (v:vs) -> patEquals pe1 v && patEquals pe2 (VList vs)
        VList [] -> False
        _ -> error "todo" -- TODO
    PEPat (PatVar _) -> True
    PEPat (PatTConstr _ _) -> undefined
    PEPat (PatWild) -> True
    PEPat (PatTrue) -> case val of
        VBool b -> b == True
        _ -> error "coś tam" -- TODO
    PEPat (PatFalse) -> case val of
        VBool b -> b == False
        _ -> error "trutututu tam" -- TODO
    PEPat (PatEList) -> case val of
        VList [] -> True
        VList (_:_) -> False
        _ -> error "ttruruiirwpe" -- TODO
    PEPat (PatInt x) -> case val of
        VInt y -> x == y
        _ -> error " xxxx " -- TODO
