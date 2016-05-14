{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeChecker.FTV where

import TypeChecker.Types
import Control.Monad.Reader
import Data.IORef
import Data.List

class HasFTV a where
    ftv :: a -> Tc [TypeVar]

instance HasFTV TypeVar where
    ftv tv@(TV _ ioref) = liftIO (readIORef ioref) >>= maybe (return [tv]) ftv

instance HasFTV Type where
    ftv (TypeVar var) = ftv var
    ftv (TypeConstr _ ts) = do
        ftv ts

instance HasFTV a => HasFTV [a] where
    ftv args = (nub . concat) <$> mapM ftv args

instance HasFTV TypeScheme where
    ftv (Forall tvs t) = do
        tftv <- ftv t
        return $ tftv \\ tvs