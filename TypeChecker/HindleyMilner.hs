{-#LANGUAGE LambdaCase#-}

module TypeChecker.HindleyMilner where

import Data.IORef
import Data.List
import Control.Monad.Reader
import qualified Data.Map as M
import qualified AbsSFL as SFL
import Data.Maybe
import TypeChecker.Types
import TypeChecker.FTV
import TypeChecker.HMUtils


infer :: Exp -> Tc Type
infer (EVar x) = do
    mts <- asks $ M.lookup x
    case mts of
        Just ts -> instantiate ts
        Nothing -> error "brak zmiennej!"

infer (EApp e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    b <- fresh
    let tb = TypeVar b
    unify t1 (TypeConstr "->" [t2, tb])
    return tb

infer (ELam x e) = do
    b <- fresh
    let tb = TypeVar b
    t <- local (M.insert x (Forall [] tb)) $ infer e
    return $ TypeConstr "->" [tb, t]

infer (ELet patExp e body) = do
    t <- infer e
    modifications <- zzz patExp t
    local modifications $ infer body

infer (EInt _) = return $ tInt

infer (EBool _) = return $ tBool

infer (EConstr name es) = do
    x <- mapM infer es
    return $ TypeConstr name x

zzz :: SFL.PatExp -> Type -> Tc (Env -> Env)
zzz patExp ttt = do
    zonked <- liftIO $ zonk ttt
    case patExp of
        SFL.PETuple pe1 pe2 -> case zonked of
            TypeConstr "tuple" [t1, t2] -> do
                z1 <- zzz pe1 t1
                z2 <- zzz pe2 t2
                return (z1 . z2)
            _ -> error "wrong tuple matching"
        SFL.PECons pe1 pe2 -> case zonked of
            TypeConstr "list" xxxx -> case xxxx of
                [lt] -> do
                    ss <- liftIO $ showType zonked
                    _ <- liftIO $ putStrLn $ "test: " ++ ss
                    z1 <- zzz pe1 lt
                    z2 <- zzz pe2 $ TypeConstr "list" [lt]
                    return (z1 . z2)
                sss -> error $ "xd: " ++ show  (length sss)
            _ -> error $ "wrong cons matching"
        SFL.PEPat (SFL.PatIdent (SFL.Ident name)) -> do
            ts <- generalize zonked
            return (M.insert name ts)
        SFL.PEPat (SFL.PatTCPat (SFL.UIdent name) pats) -> error "TCPAT"
        SFL.PEPat (SFL.PatWild) -> return id


instantiate :: TypeScheme -> Tc Type
instantiate (Forall tvs t) = do
    tvs' <- mapM (const fresh) tvs
    let subst = zip tvs tvs'
    return $ applySubstr subst t

generalize :: Type -> Tc TypeScheme
generalize t = do
    fv <- ftv t
    m <- ask
    fvenv <- ftv $ M.elems m
    return $ Forall (fv \\ fvenv) t



unify :: Type -> Type -> Tc ()
unify (TypeVar tv) t' = unifyVar tv t'
unify t (TypeVar tv') = unifyVar tv' t
unify (TypeConstr name args) (TypeConstr name' args')
    | name == name' = zipWithM_ unify args args'
    | otherwise = fail $ "nie umim w inferowanie: " ++ name ++ " vs. " ++ name'

unifyVar :: TypeVar -> Type -> Tc ()
unifyVar ioref t = do
    liftIO (readIORef ioref) >>= \case
        Just context -> unify context t
        Nothing -> do
            zonked <- liftIO $ zonk t
            if occursCheck ioref zonked then
                fail "occurs check failed <corner!?>"
            else
                liftIO $ writeIORef ioref (Just zonked)