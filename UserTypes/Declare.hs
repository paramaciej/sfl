{-# LANGUAGE LambdaCase #-}

module UserTypes.Declare where

import qualified AbsSFL as SFL
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Exceptions.TypeErrors
import TypeChecker.Types
import TypeChecker.Utils

declareType :: SFL.Stmt -> Tc (Env -> Env)
declareType = \case
    SFL.TypeDecl (SFL.UIdent tName) params constructors -> do
        typeNameFree <- asks (M.notMember tName . typeDefs)
        if typeNameFree
            then if length (nub $ map getTCName constructors) == length constructors
                then do
                    modifications <- mapM (addTypeConstr tName params) constructors
                    return $ foldr (.) id modifications
                else throwError $ DuplicatedConstructors
            else throwError $ TypeDeclared tName
    _ -> error "type declaration with different expression"

getFrees :: [SFL.Ident] -> Tc [(String, TypeVar)]
getFrees = mapM $ \(SFL.Ident name) -> do
    free <- fresh
    return (name, free)

addTypeConstr :: String -> [SFL.Ident] -> SFL.TC -> Tc (Env -> Env)
addTypeConstr tName params typeConstr = do
    let constrName = getTCName typeConstr
    constrNameFree <- asks (M.notMember constrName . typeConstrs)
    if constrNameFree
        then do
            constrTypeScheme <- case typeConstr of
                SFL.TConstrS _ -> do
                    frees <- getFrees params
                    generalize $ TypeConstr tName $ varsFromFrees frees
                SFL.TConstr _ tcArgs -> do
                    frees <- getFrees params
                    argTypes <- forM tcArgs $ \case
                        SFL.TCAIdent (SFL.Ident tcArgName) -> case lookup tcArgName frees of
                            Just free -> return $ TypeVar free
                            Nothing -> throwError $ UndefinedError tcArgName
                        SFL.TCAArg arg -> local (\(Env mIO sm tc td) -> Env mIO sm tc (M.insert tName (length params) td))
                            $ resolveTC frees arg
                    generalize $ foldr (\x acc -> TypeConstr "->" [x, acc]) (TypeConstr tName $ varsFromFrees frees) argTypes

            return $ \(Env m sm tcs tds) -> Env m sm
                (M.insert constrName (TCEntry tName constrTypeScheme) tcs)
                (M.insert tName (length params) tds)
        else throwError $ ConstrDeclared constrName
  where
    varsFromFrees = map $ \(_, tv) -> TypeVar tv

getTCName :: SFL.TC -> String
getTCName (SFL.TConstrS (SFL.UIdent name)) = name
getTCName (SFL.TConstr (SFL.UIdent name) _) = name

resolveTC :: [(String, TypeVar)] -> SFL.TC -> Tc Type
resolveTC frees = \case
    SFL.TConstrS (SFL.UIdent name) -> resolve name []
    SFL.TConstr (SFL.UIdent name) args -> resolve name args
  where
    resolve name args = do
        mType <- asks (M.lookup name . typeDefs)
        case mType of
            Just nArgs -> if nArgs == length args
                then do
                    argTypes <- forM args $ \case
                        SFL.TCAIdent (SFL.Ident n) -> case lookup n frees of
                            Just x -> return $ TypeVar x
                            Nothing -> throwError $ UndefinedError n
                        SFL.TCAArg tc -> resolveTC frees tc
                    return $ TypeConstr name argTypes
                else throwError $ TypeArgsNumber name nArgs (length args)
            Nothing -> throwError $ UndefinedError name
