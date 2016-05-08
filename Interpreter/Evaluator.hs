{-#LANGUAGE LambdaCase#-}

module Interpreter.Evaluator where

import Control.Monad.Reader
import TypeChecker.Types
import TypeChecker.HMUtils
import qualified Data.Map as M
import StdLib.Operators
import Interpreter.Types
import Interpreter.PatEval



eval :: Exp -> VE Value
eval = \case
    EVar name -> do
        m <- ask
        return $ m M.! name
    EApp e1 e2 -> do
        val <- eval e1
        case val of
            VFun fun -> do
                arg <- eval e2
                fun arg
            _ -> error "GUPIA APPL."
    ELam name body -> do
        return $ VFun (\xx -> local (M.insert name xx) $ eval body)
    ELet patExp e body -> do
        val <- eval e
        modifications <- patEval patExp val
        local modifications $ eval body
    EInt n -> return $ VInt n
    EBool b -> return $ VBool b
    EConstr "tuple" exps -> do
        vals <- mapM eval exps
        return $ VTuple vals
    EConstr name exps -> do
        vals <- mapM eval exps
        return $ VConstr name vals
