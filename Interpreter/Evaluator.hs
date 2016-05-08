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
                return $ fun arg
            _ -> error "GUPIA APPL."
    EInt n -> return $ VInt n
    EBool b -> return $ VBool b
    EConstr "tuple" exps -> do
        vals <- mapM eval exps
        return $ VTuple vals
    EConstr name exps -> do
        vals <- mapM eval exps
        return $ VConstr name vals
    ELet patExp e body -> do
        val <- eval e
        modifications <- patEval patExp val
        local modifications $ eval body

--    ELet name e body -> do
--        val <- eval e
--        local (M.insert name val) $ eval body
    _ -> return $ VBool False
