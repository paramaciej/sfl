{-#LANGUAGE LambdaCase#-}
module TypeChecker.Infer where

import qualified AbsSFL as SFL
import TypeChecker.Types
import Control.Monad.Reader
import qualified Data.Map as M
import TypeChecker.HindleyMilner
import TypeChecker.HMUtils
import StdLib.Operators

tcExp :: SFL.Exp -> Exp
tcExp = \case
--    SFL.ELam
--    SFL.EMatch
    SFL.ETuple e1 e2 -> EConstr "tuple" [tcExp e1, tcExp e2]
    SFL.ECons e1 e2 -> EApp (EApp (EVar "cons") (tcExp e1)) (tcExp e2)
    SFL.EInt n -> EInt n
    SFL.ETrue -> EConstr "Bool" []
    SFL.EFalse -> EConstr "Bool" []
    SFL.EAdd e1 e2 -> mulEApp (EVar "plus") (tcExp <$> [e1, e2])
    SFL.EApp e args -> mulEApp (tcExp e) (tcExp <$> args)
    SFL.EVar (SFL.Ident name) -> EVar name
    SFL.EList lElems -> foldr (\(SFL.EListElem e) acc -> mulEApp (EVar "cons") [tcExp e, acc]) (EVar "[]") lElems

    _ -> error "lololol"

inferredType :: SFL.Exp -> IO Type
inferredType e = do
    env <- StdLib.Operators.ops
    runReaderT (infer $ tcExp e) env