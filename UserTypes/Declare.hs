{-# LANGUAGE LambdaCase #-}

module UserTypes.Declare where

import qualified AbsSFL as SFL
import Control.Monad.Reader
import TypeChecker.Types
import qualified Data.Map as M
import TypeChecker.Utils

declareType :: SFL.Stmt -> Tc (Env -> Env)
declareType = \case
    SFL.TypeDecl (SFL.UIdent typeName) params constructors -> do
--        decls <- gets (typeConstrs . types)
        Env _ _ tcs <- ask

        liftIO $ putStrLn $ "no elo. declared constrs: " ++ unlines (M.keys tcs)

        modifications <- mapM (xxxTC typeName params) constructors
        return $ foldr (.) id modifications
    _ -> error "type declaration with other expression"

getFrees :: [SFL.Ident] -> Tc [(String, TypeVar)]
getFrees = mapM $ \(SFL.Ident name) -> do
    free <- fresh
    return (name, free)

xxxTC :: String -> [SFL.Ident] -> SFL.TC -> Tc (Env -> Env)
xxxTC typeName params = \case
    SFL.TConstrS (SFL.UIdent constrName) -> do
        constrTypeScheme <- do
            frees <- getFrees params
            generalize $ TypeConstr typeName $ varsFromFrees frees
        return $ \(Env m sm tcs) -> Env m sm (M.insert constrName (TCEntry typeName constrTypeScheme) tcs)
    SFL.TConstr (SFL.UIdent constrName) tcArgs -> do
        constrTypeScheme <- do
            frees <- getFrees params

            xtypes <- forM tcArgs $ \case
                SFL.TCAIdent (SFL.Ident tcArgName) -> case lookup tcArgName frees of
                    Just x -> return $ TypeVar x
                    Nothing -> error "TODO UNDEF" -- TODO
                SFL.TCAArg tc -> error "TODO" -- TOOD

            generalize $ foldr (\x acc -> TypeConstr "->" [x, acc]) (TypeConstr typeName $ varsFromFrees frees) xtypes
        return $ \(Env m sm tcs) -> Env m sm (M.insert constrName (TCEntry typeName constrTypeScheme) tcs)
  where
    varsFromFrees = map $ \(_, tv) -> TypeVar tv

--    SFL.TConstr (SFL.UIdent constrName)
--
--hhTC typeName params = \case
--    (SFL.TConstr (SFL.UIdent constrName) tcArgs) -> do
--        liftIO $ putStrLn $ "TC: " ++ constrName
--        let func ts = if length ts == length tcArgs
--            then do -- TODO dostaję ts!
--                frees <- getFrees params
--                mapM_ (\(t, tcArg) -> case tcArg of
--                    SFL.TCAIdent (SFL.Ident tcArgName) -> case lookup tcArgName frees of
--                        Just x -> unify (TypeVar x) t
--                        Nothing -> error "undefined ???" -- TODO
--                    SFL.TCAArg tc -> undefined
--                    ) $ zip ts tcArgs
--
--        --                      TODO \/
--                xtypes <- mapM (\case
--                    SFL.TCAIdent (SFL.Ident tcArgName) -> case lookup tcArgName frees of
--                        Just x -> return $ TypeVar x
--                        Nothing -> error "undefined ???" -- TODO
--                    SFL.TCAArg tc -> error "TODO" -- TODO
--                    ) $ drop (length ts) tcArgs
--        --                     mapM_ (\tcArg -> \case
--        --                        SFL.TCAIdent _ -> return ()
--        --                        SFL.TCAArg tc -> error "drop / aarg error"
--        --                        ) $ drop (length ts) tcArgs
--        --                      TODO ^^
--                return $ foldl (\acc xtype -> TypeConstr "->" [xtype, acc]) (TypeConstr typeName (map (\(_, tv) -> TypeVar tv) frees)) (xtypes)
--
--        --                    return $ TypeConstr typeName (map (\(_, tv) -> TypeVar tv) frees)
--        --                    return ts
--            else throwError $ ConstructorArgsNumber constrName (length tcArgs) (length ts)
--
--        return $ \(Env m sm tcs') -> Env m sm (M.insert constrName (TCEntry typeName func) tcs')
--    (SFL.TConstrS (SFL.UIdent constrName)) -> error "XD" -- TODO