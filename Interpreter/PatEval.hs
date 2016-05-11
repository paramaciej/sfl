{-# LANGUAGE LambdaCase #-}
module Interpreter.PatEval where

import Interpreter.Types
import AbsSFL as SFL
import Data.Map

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