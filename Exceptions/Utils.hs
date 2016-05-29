module Exceptions.Utils where

import TypeChecker.Types
import TypeChecker.Show
import Control.Monad.Except
import Exceptions.TypeErrors
import AbsSFL

raiseMismatchError :: Type -> Type -> Maybe TypeError -> Tc ()
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


catchAppInfer :: Tc() -> Type -> Type -> Tc ()
catchAppInfer unify funT argT =
    catchError unify handle where
        handle err = do
            funStr <- showType funT
            argStr <- showType argT
            throwError $ ApplicationTypeError funStr argStr err

catchIfInfer :: Tc Type -> Tc Type
catchIfInfer ifInfer =
    catchError ifInfer handle where
        handle :: TypeError -> Tc Type
        handle err = throwError $ InferError "Type error in if expression." err

catchMatchInfer :: Tc () -> Tc ()
catchMatchInfer unify =
    catchError unify handle where
        handle :: TypeError -> Tc ()
        handle err = throwError $ DifferentCaseTypesError err

catchPatternMatch :: Tc () -> PatExp -> Type -> Tc ()
catchPatternMatch unify patExp t =
    catchError unify handle where
        handle err = do
            tStr <- showType t
            throwError $ PatternMatchingError (show patExp) tStr err