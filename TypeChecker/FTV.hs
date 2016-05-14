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
--    ftv tv = liftIO (readIORef tv) >>= maybe (return [tv]) ftv
    ftv tv@(TV _ ioref) = liftIO (readIORef ioref) >>= maybe (return [tv]) ftv

instance HasFTV Type where
    ftv (TypeVar var) = ftv var
    ftv (TypeConstr name ts) = do
        xx <- ftv ts
--        liftIO $ putStrLn $ "FTV TYPE: " ++ name ++ ",  len: " ++ show (length xx)
        if length xx == 2 then
            let a:b:[] = xx in liftIO $ putStrLn $ "\t!!! FTV 2: " ++ show (a==b)
        else liftIO $ putStr ""
        return xx

instance HasFTV a => HasFTV [a] where
    ftv args = (nub . concat) <$> mapM ftv args

instance HasFTV TypeScheme where
    ftv (Forall tvs t) = do
--        liftIO $ putStrLn $ "FTV TYPE SCHEME! \ttvs:" ++ show (length tvs)
        tftv <- ftv t
--        liftIO $ putStrLn $ "\t\t\tftv:" ++ show (length tftv)
        return $ tftv \\ tvs