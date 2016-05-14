module Interpreter.Utils where

import AbsSFL as SFL
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Interpreter.Evaluator
import Interpreter.Types
import TypeChecker.HindleyMilner
import TypeChecker.Infer
import TypeChecker.Types
import TypeChecker.Utils
import PrintSFL
import TypeChecker.Show

inferredType :: SFL.Exp -> PrSt Type
inferredType e = do
    env <- gets types
    let inferError str =  str ++ "\n\tin\n\t>" ++ printTree e
    liftIO (runReaderT (runExceptT (infer $ tcExp e)) env) >>= either (throwError . inferError) (liftIO . zonk)

typeStr :: SFL.Exp -> PrSt String
typeStr e = do
    t <- inferredType e
--    gets types >>= liftIO . runReaderT (showType t)
    tenv <- gets types
    liftIO (runReaderT (runExceptT (showType t)) tenv) >>= either throwError return

evaluatedExp :: SFL.Exp -> PrSt Value
evaluatedExp e = do
    env <- gets values
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