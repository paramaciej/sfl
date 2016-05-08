{-#LANGUAGE LambdaCase#-}
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
        _ -> error $ "wrong value for tuple: " ++ show val
    PECons pe1 pe2 -> case val of
        VList (v:vs) -> do
            ev1 <- patEval pe1 v
            ev2 <- patEval pe2 (VList vs)
            return (ev1 . ev2)
        _ -> error $ "wrong value for list: " ++ show val
    PEPat (PatIdent (Ident name)) -> return $ insert name val
    -- PatTCPat
    PEPat (PatWild) -> return id