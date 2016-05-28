{-# LANGUAGE LambdaCase #-}
module Interpreter.PatEval where

import Interpreter.Types
import AbsSFL as SFL
import qualified Data.Map as M
import Control.Monad.Except
import Exceptions.EvalErrors

patEval :: SFL.PatExp -> Value -> VE (ValEnv -> ValEnv)
patEval patExp val = case patExp of
    PETuple pe1 pe2 -> case val of
        VTuple [v1, v2] -> do
            ev1 <- patEval pe1 v1
            ev2 <- patEval pe2 v2
            return (ev1 . ev2)
        _ -> error $ "wrong value for tuple!"
    PECons pe1 pe2 -> case val of
        VList (v:vs) -> do
            ev1 <- patEval pe1 v
            ev2 <- patEval pe2 (VList vs)
            return (ev1 . ev2)
        VList [] -> error $ "matching with empty list!"
        _ -> error $ "wrong value for list!"
    PEPat (PatVar (Ident name)) -> return $ M.insert name val
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
    PEPat (PatList elems) -> do
        let patExps = map (\(PatLElem p) -> p) elems
        case val of
            VList vals -> if length patExps == length vals
                then do
                    evs <- mapM (\(p, v) -> patEval p v) (zip patExps vals)
                    return $ foldr (.) id evs
                else throwError $ ListPatternLengthMismatch (length patExps) (length vals)
            _ -> error $ "wrong value for list: " ++ show val
    PEPat (PatInt _) -> return id




patEquals :: SFL.PatExp -> Value -> Bool
patEquals patExp val = case patExp of
    PETuple pe1 pe2 -> case val of
        VTuple [v1, v2] -> patEquals pe1 v1 && patEquals pe2 v2
        _ -> error "impossible at patEquals tuple"
    PECons pe1 pe2 -> case val of
        VList (v:vs) -> patEquals pe1 v && patEquals pe2 (VList vs)
        VList [] -> False
        _ -> error "impossible at patEquals cons"
    PEPat (PatVar _) -> True
    PEPat (PatTConstr (UIdent name) pats) -> case val of
        VConstr name' vals -> if name == name'
            then all (\(p, v) -> patEquals p v) (zip pats vals)
            else False
        _ -> error "impossible at patEquals type constr"
    PEPat (PatWild) -> True
    PEPat (PatTrue) -> case val of
        VBool b -> b == True
        _ -> error "impossible at patEquals bool"
    PEPat (PatFalse) -> case val of
        VBool b -> b == False
        _ -> error "impossible at patEquals bool"
    PEPat (PatList elems) -> let patExps = map (\(PatLElem p) -> p) elems
        in case val of
            VList vals -> if length patExps == length vals
                then all (\(p, v) -> patEquals p v) (zip patExps vals)
                else False
            _ -> error "impossible at patEquals list"
    PEPat (PatInt x) -> case val of
        VInt y -> x == y
        _ -> error "impossible at patEquals int"
