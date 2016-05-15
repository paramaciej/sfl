{-# LANGUAGE LambdaCase #-}
module TypeChecker.Utils where

import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.List
import qualified Data.Map as M
import TypeChecker.Types
import TypeChecker.FTV


envInsert :: String -> TypeScheme -> Env -> Env
envInsert name value (Env m t tc) = Env m (M.insert name value t) tc


zonk :: Type -> IO Type
zonk (TypeConstr name ts) = TypeConstr name <$> mapM zonk ts
zonk t@(TypeVar (TV _ ioref)) = liftIO (readIORef ioref) >>= \case
    Nothing -> return t
    Just ty -> zonk ty


fresh :: Tc TypeVar
fresh = do
    cRef <- asks maxIORef
    counter <- liftIO $ (+1) <$> readIORef cRef
    liftIO $ writeIORef cRef counter
    ioref <- liftIO (newIORef Nothing)
    return $ TV counter ioref


applySubstr :: [(TypeVar, TypeVar)] -> Type -> Type
applySubstr subst (TypeVar tv) = TypeVar $ fromMaybe tv (lookup tv subst)
applySubstr subst (TypeConstr name ts) = TypeConstr name $ map (applySubstr subst) ts


occursCheck :: TypeVar -> Type -> Bool
occursCheck ioref (TypeVar tv) = ioref == tv
occursCheck ioref (TypeConstr _ ts) = any (occursCheck ioref) ts


generalize :: Type -> Tc TypeScheme
generalize t = do
    fv <- ftv t
    m <- asks schemeMap
    fvenv <- ftv $ M.elems m
    return $ Forall (fv \\ fvenv) t


mulEApp :: Exp -> [Exp] -> Exp
mulEApp _ [] = error "Application with no arguments!"
mulEApp fun args = foldl EApp fun args

tApp :: Type -> Type -> Type
tApp arg ret = TypeConstr "->" [arg, ret]

mulTApp :: [Type] -> Type -> Type
mulTApp args ret = foldr tApp ret args
