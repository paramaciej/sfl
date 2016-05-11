{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Interpreter.Evaluator where

import Control.Monad.Reader
import TypeChecker.Types
import qualified Data.Map as M
import Interpreter.Types
import Interpreter.PatEval



eval :: Exp -> VE Value
eval = \case
    EVar name -> do
        m <- ask
        case M.lookup name m of
            Just v -> return v
            Nothing -> error $ "'" ++ name ++ "' not found. :/" -- FIXME
    EApp e1 e2 -> do
        val <- eval e1
        case val of
            VFun fun -> do
                arg <- eval e2
                fun arg
            _ -> error "a non-function value used as a function in application!"
    ELam name body -> do
        env <- ask
        return $ VFun (\xx -> local ((M.insert name xx) . (const env)) $ eval body)
    ELet patExp e body -> do
        val <- eval e
        modifications <- patEval patExp val
        local modifications $ eval body
    ELetRec name e body -> do
        rec val <- local (M.insert name val) $ eval e
        local (M.insert name val) $ eval body

    EInt n -> return $ VInt n
    EBool b -> return $ VBool b
    EIf cond eTrue eFalse -> do
        isTrue <- eval cond
        case isTrue of
            VBool True -> eval eTrue
            VBool False -> eval eFalse
            _ -> error "conditional doesn't evaluate to Bool!"
--    EMatch e cases -> do
--        val <- eval e
    EConstr "tuple" exps -> do
        vals <- mapM eval exps
        return $ VTuple vals
    EConstr name exps -> do
        vals <- mapM eval exps
        return $ VConstr name vals
