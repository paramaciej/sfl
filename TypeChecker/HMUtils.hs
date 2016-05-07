{-#LANGUAGE LambdaCase#-}
module TypeChecker.HMUtils where

import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import TypeChecker.Types


zonk :: Type -> Tc Type
zonk (TypeConstr name ts) = TypeConstr name <$> mapM zonk ts
zonk t@(TypeVar tv) = liftIO (readIORef tv) >>= \case
    Nothing -> return t
    Just ty -> zonk ty


fresh :: Tc (TypeVar)
fresh = liftIO (newIORef Nothing)


applySubstr :: [(TypeVar, TypeVar)] -> Type -> Type
applySubstr subst (TypeVar tv) = TypeVar $ fromMaybe tv (lookup tv subst)
applySubstr subst (TypeConstr name ts) = TypeConstr name $ map (applySubstr subst) ts


occursCheck :: TypeVar -> Type -> Bool
occursCheck ioref (TypeVar tv) = ioref == tv
occursCheck ioref (TypeConstr _ ts) = any (occursCheck ioref) ts
