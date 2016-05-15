{-# LANGUAGE LambdaCase #-}

module UserTypes.Declare where

import qualified AbsSFL as SFL
import Interpreter.Types
import Control.Monad.Reader
import TypeChecker.Types
import qualified Data.Map as M
import TypeChecker.Utils
import TypeChecker.HindleyMilner
import Exceptions.Types
import Control.Monad.Except

declareType :: SFL.Stmt -> Tc (Env -> Env)
declareType = \case
    SFL.TypeDecl (SFL.UIdent typeName) params constructors -> do
--        decls <- gets (typeConstrs . types)
        Env _ _ tcs <- ask

        liftIO $ putStrLn $ "no elo. declared constrs: " ++ unlines (M.keys tcs)

        let hTC (SFL.TypeConstr (SFL.UIdent constrName) tcArgs) = do

            liftIO $ putStrLn $ "TC: " ++ constrName
            let func ts = if length ts == length tcArgs
                then do
                    frees <- mapM (\(SFL.Ident n) -> do
                        fr <- fresh
                        return (n, fr)) params
                    mapM_ (\(t, tcArg) -> case tcArg of
                        SFL.TCAIdent (SFL.Ident tcArgName) -> case lookup tcArgName frees of
                            Just x -> unify (TypeVar x) t
                            Nothing -> error "undefined ???" -- TODO
                        SFL.TCAArg tc -> undefined
                        ) $ zip ts tcArgs
                    return $ TypeConstr typeName (map (\(_, tv) -> TypeVar tv) frees)
--                    return ts
                else throwError $ ConstructorArgsNumber constrName (length tcArgs) (length ts)

            return $ \(Env m sm tcs') -> Env m sm (M.insert constrName (TCEntry typeName func) tcs')

        modifications <- mapM hTC constructors
        return $ foldr (.) id modifications
    _ -> error "type declaration with other expression"