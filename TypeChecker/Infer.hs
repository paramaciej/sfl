{-# LANGUAGE LambdaCase #-}
module TypeChecker.Infer where

import qualified AbsSFL as SFL
import TypeChecker.Types
import TypeChecker.Utils

tcExp :: SFL.Exp -> Exp
tcExp = \case
    SFL.ELam (SFL.Ident name) body -> case body of
        SFL.FBodyMatch _ -> error "not yet implemented!"  -- TODO
        SFL.FBodyExp e -> ELam name (tcExp e)

    SFL.EMatch e (SFL.MBody cases) -> EMatch (tcExp e) (map (\(SFL.MCase patExp body) -> (patExp, tcExp body)) cases)
    SFL.EIf cond e1 e2 -> EIf (tcExp cond) (tcExp e1) (tcExp e2)
    SFL.ELet patExp e body -> ELet patExp (tcExp e) (tcExp body)
    SFL.ELetRec (SFL.Ident name) e body -> ELetRec name (tcExp e) (tcExp body)
    SFL.ETuple e es -> EConstr "tuple" (tcExp <$> e:es)
    SFL.ECons e1 e2 -> EApp (EApp (EVar "cons") (tcExp e1)) (tcExp e2)
    SFL.EOr e1 e2 -> fapp "_or" [e1, e2]
    SFL.EAnd e1 e2 -> fapp "_and" [e1, e2]
    SFL.ENot e -> fapp "_not" [e]
    SFL.ELt e1 e2 -> fapp "_lt" [e1, e2]
    SFL.ELte e1 e2 -> fapp "_lte" [e1, e2]
    SFL.EGt e1 e2 -> fapp "_gt" [e1, e2]
    SFL.EGte e1 e2 -> fapp "_gte" [e1, e2]
    SFL.EEq e1 e2 -> fapp "_eq" [e1, e2]
    SFL.ENEq e1 e2 -> fapp "_neq" [e1, e2]
    SFL.EAdd e1 e2 -> fapp "add" [e1, e2]
    SFL.ESub e1 e2 -> fapp "sub" [e1, e2]
    SFL.EMul e1 e2 -> fapp "mul" [e1, e2]
    SFL.EDiv e1 e2 -> fapp "div" [e1, e2]
    SFL.EMod e1 e2 -> fapp "mod" [e1, e2]
    SFL.EApp e args -> mulEApp (tcExp e) (tcExp <$> args)
    SFL.ELiteral (SFL.LInt n) -> EInt n
    SFL.ELiteral SFL.LTrue -> EBool True
    SFL.ELiteral SFL.LFalse -> EBool False
    SFL.ELiteral (SFL.LList lElems) -> foldr (\(SFL.EListElem e) acc -> mulEApp (EVar "cons") [tcExp e, acc]) (EVar "[]") lElems
    SFL.ELiteral (SFL.LVar (SFL.Ident name)) -> EVar name
    SFL.ELiteral (SFL.LTConstr (SFL.UIdent name) args) -> EConstr name ((tcExp . (\(SFL.ETCElem e) -> e))<$> args)

fapp :: String -> [SFL.Exp] -> Exp
fapp name exps = mulEApp (EVar name) (tcExp <$> exps)
