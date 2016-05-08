{-#LANGUAGE LambdaCase#-}
module TypeChecker.HMUtils where

import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import TypeChecker.Types
import qualified AbsSFL as SFL


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


showType :: Type -> IO String
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



--yyy :: SFL.PatExp -> Type -> Exp -> Tc Type
--yyy patExp t body = case patExp of
--    SFL.PETuple pe1 pe2 -> case t of
--        TypeConstr "tuple" [t1, t2] -> do
--            yyy pe1 t1
--    SFL.PECons pe1 pe2 ->
--    SFL.PEPat (SFL.PatIdent (SFL.Ident name)) -> do
--        ts <- generalize t
--        local (M.insert name ts) $ infer body
--    SFL.PEPat (SFL.PatTCPat (SFL.UIdent name) pats) -> error "TCPAT"
--    SFL.PEPat (SFL.PatWild) -> infer body
--
--    do
--    ts <- generalize t
--    local (M.insert ??? ts) $ infer body


mulEApp :: Exp -> [Exp] -> Exp
mulEApp _ [] = error "Application with no arguments!"
mulEApp fun args = foldl (\acc arg -> EApp acc arg) fun args

tApp :: Type -> Type -> Type
tApp arg ret = TypeConstr "->" [arg, ret]

mulTApp :: [Type] -> Type -> Type
mulTApp args ret = foldr (\arg acc -> tApp arg acc) ret args
