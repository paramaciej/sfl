module TypeChecker.Infer where

import qualified AbsSFL as SFL
import TypeChecker.Types
import Control.Monad.Reader
import Data.Map
import TypeChecker.HindleyMilner
import TypeChecker.HMUtils
import StdLib.Operators

tcExp :: SFL.Exp -> Exp
tcExp (SFL.ETuple e1 e2) = EConstr "tuple" [tcExp e1, tcExp e2]
tcExp (SFL.ECons e1 e2) = EConstr ":" [tcExp e1, tcExp e2]
tcExp (SFL.EInt n) = EInt n
tcExp (SFL.ETrue) = EConstr "Bool" []
tcExp (SFL.EFalse) = EConstr "Bool" []
tcExp (SFL.EAdd e1 e2) = EApp (EApp (EVar "plus") (tcExp e1)) (tcExp e2)
tcExp (SFL.EApp e args) = mulEApp (tcExp e) (tcExp <$> args)
tcExp (SFL.EVar (SFL.Ident name)) = EVar name


inferredType :: SFL.Exp -> IO Type
inferredType e = runReaderT (infer $ tcExp e) StdLib.Operators.ops