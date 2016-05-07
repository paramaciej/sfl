{-#LANGUAGE LambdaCase#-}
module TypeChecker.HMUtils where

import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import TypeChecker.Types


zonk :: Type -> IO Type
zonk (TypeConstr name ts) = TypeConstr name <$> mapM zonk ts
zonk t@(TypeVar tv) = liftIO (readIORef tv) >>= \case
    Nothing -> return t
    Just ty -> zonk ty


fresh :: MonadIO m => m TypeVar
fresh = liftIO (newIORef Nothing)


applySubstr :: [(TypeVar, TypeVar)] -> Type -> Type
applySubstr subst (TypeVar tv) = TypeVar $ fromMaybe tv (lookup tv subst)
applySubstr subst (TypeConstr name ts) = TypeConstr name $ map (applySubstr subst) ts


occursCheck :: TypeVar -> Type -> Bool
occursCheck ioref (TypeVar tv) = ioref == tv
occursCheck ioref (TypeConstr _ ts) = any (occursCheck ioref) ts


showType :: Type -> IO String -- TODO zonk?

-------------
showType t = do
    zonked <- zonk t
    case zonked of
        TypeConstr name ts -> do
            tsStr <- mapM showType ts
            return $ case name of
                "->" -> case tsStr of
                    [left, right] -> left ++ " -> " ++ right
                    _ -> error "Wrong number of arguments in application!"
                "list" -> case tsStr of
                    [str] -> "[" ++ str ++ "]"
                    _ -> error "Wrong number of arguments in list type!"
                _ -> case tsStr of
                    [] -> name
                    _ -> "(" ++ name ++ concatMap (" " ++) tsStr ++ ")"
        TypeVar _ -> return "Free"



mulEApp :: Exp -> [Exp] -> Exp
mulEApp _ [] = error "Application with no arguments!"
mulEApp fun args = foldl (\acc arg -> EApp acc arg) fun args

tApp :: Type -> Type -> Type
tApp arg ret = TypeConstr "->" [arg, ret]