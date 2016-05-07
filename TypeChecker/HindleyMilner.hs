{-#LANGUAGE LambdaCase#-}

module TypeChecker.HindleyMilner where

import Data.IORef
import Data.List
import Control.Monad.Reader
import qualified Data.Map as M
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

infer (ELet x e body) = do
    t <- infer e
    ts <- generalize t
    local (M.insert x ts) $ infer body

infer (EInt _) = return $ TypeConstr "Int" []

infer (EConstr name es) = do
    x <- mapM infer es
    return $ TypeConstr name x


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