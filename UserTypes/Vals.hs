{-# LANGUAGE LambdaCase #-}

module UserTypes.Vals where

import qualified AbsSFL as SFL
import qualified Data.Map as M
import Interpreter.Types

defineConstrs :: SFL.Stmt -> VE (ValEnv -> ValEnv)
defineConstrs = \case
    SFL.TypeDecl (SFL.UIdent typeName) params constructors -> do
        modifications <- mapM (yyyTC typeName params) constructors
        return $ foldr (.) id modifications
    _ -> error "type decl. with other expr" -- TODO

yyyTC :: String -> [SFL.Ident] -> SFL.TC -> VE (ValEnv -> ValEnv)
yyyTC typeName params = \case
    SFL.TConstrS (SFL.UIdent constrName) -> return $ M.insert constrName $ VConstr constrName []
    SFL.TConstr (SFL.UIdent constrName) tcArgs -> return $ M.insert constrName $ mkCtor constrName (length tcArgs)

mkCtor :: String -> Int ->  Value
mkCtor name = go []
  where
    go args 0 = VConstr name $ reverse args
    go args n = VFun $ \arg -> return $ go (arg : args) (n - 1)
