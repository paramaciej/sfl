module Exceptions.Utils where

import TypeChecker.Types
import TypeChecker.Show
import Control.Monad.Except
import Exceptions.Types
import Data.Maybe

raiseMismatchError :: Type -> Type -> Maybe TypeException -> Tc ()
raiseMismatchError t t' mTerr = do
    name <- showType t
    name' <- showType t'
    throwError $ TypeMismatchError $ MismatchError name name' $ case mTerr of
        Nothing -> Nothing
        Just (TypeMismatchError mErr) -> Just mErr
        _ -> error "Wrong TypeException in MismatchError!"

raiseOccursCheckError :: TypeVar -> Type -> Tc ()
raiseOccursCheckError tv t = do
    tStr <- showType t
    throwError $ OccursCheckError (show tv) tStr

--bubbleMismatchError :: Type -> Type -> TypeException -> Tc ()
--bubbleMismatchError t t' err = do
--    throwError

--catchApplicationError :: _
--catchApplicationError x = do
--    catchError x aux
--  where
--    aux txt = do
--        str1 <- return "A" -- showType t1
--        str2 <- return "B" -- showType t2
--        throwError $ "Próba aplikacji " ++ str2 ++ " do funkcji " ++ str1 ++ "\n Błąd: " ++ txt
