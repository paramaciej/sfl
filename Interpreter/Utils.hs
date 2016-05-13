module Interpreter.Utils where

import AbsSFL as SFL
import Control.Monad.Reader
import Control.Monad.State
import Interpreter.Evaluator
import Interpreter.Types
import TypeChecker.HindleyMilner
import TypeChecker.Infer
import TypeChecker.Types
import TypeChecker.HMUtils

inferredType :: SFL.Exp -> PrSt Type
inferredType e = do
    env <- gets types
    t <- liftIO $ runReaderT (infer $ tcExp e) env
    return t

typeStr :: SFL.Exp -> PrSt String
typeStr e = do
    t <- inferredType e
    tenv <- gets types
--    zonked <- liftIO $ zonk t
    sss <- liftIO $ runReaderT (showScheme t) tenv
    return sss

evaluatedExp :: SFL.Exp -> PrSt Value
evaluatedExp e = do
    env <- gets values
    liftIO $ putStrLn $ "TC: " ++ show (tcExp e)
    liftIO $ runReaderT (eval (tcExp e)) env

showExp :: SFL.Exp -> PrSt String
showExp e = do
    typ <- typeStr e
    val <- evaluatedExp e
    return $ show val ++ " : " ++ typ

printExp :: SFL.Exp -> PrSt ()
printExp e = do
    str <- showExp e
    liftIO $ putStrLn str