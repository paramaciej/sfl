{-# LANGUAGE LambdaCase #-}
module TypeChecker.HMUtils where

import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.List
import qualified Data.Map as M
import TypeChecker.Types
import TypeChecker.FTV

envInsert :: String -> TypeScheme -> Env -> Env
envInsert name value (Env m t) = Env m (M.insert name value t)


zonk :: Type -> IO Type
zonk (TypeConstr name ts) = TypeConstr name <$> mapM zonk ts
zonk t@(TypeVar (TV _ ioref)) = liftIO (readIORef ioref) >>= \case
    Nothing -> return t
    Just ty -> zonk ty


fresh :: Tc TypeVar
fresh = do
    mref <- asks maxIORef
    mcounter <- liftIO $ (+1) <$> readIORef mref
    liftIO $ writeIORef mref mcounter
--    mcounter <- liftIO $ readIORef mref
    liftIO $ putStrLn $ "\t >> FRESH nr  " ++ show (mcounter)
    ioref <- liftIO (newIORef Nothing)
    return $ TV mcounter ioref


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
        TypeVar tv -> return $ show tv


xxx :: Type -> (ReaderT ([(TypeVar, String)]) IO) String
xxx t = do
    zonked <- liftIO $ zonk t
    case zonked of
        TypeConstr name ts -> do
            tsStr <- mapM xxx ts
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
        TypeVar tv -> do
            yyy <- asks $ lookup tv
            return $ case yyy of
                Just name -> "Fr." ++ name
                Nothing -> "Fr.Unknown"

showScheme :: Type -> Tc String
showScheme t = do
    ts <- generalize t
    showSchemeX ts

showSchemeX :: TypeScheme -> Tc String
showSchemeX (Forall tvs tt) = do
    tStr <- liftIO $ runReaderT (xxx tt) (zip tvs (map (\c -> [c]) ['a'..]))
    return $ show (length tvs) ++  tStr



generalize :: Type -> Tc TypeScheme
generalize t = do
    tstr <- liftIO $ showType t
    liftIO $ putStrLn $ "\tGENERALIZE TYPE: " ++ tstr
    fv <- ftv t
    m <- asks schemeMap
    fvenv <- ftv $ M.elems m
    liftIO $ putStrLn $ "\tfree v = " ++ show (fv) ++ ", free env = " ++ show (fvenv)
    return $ Forall (fv \\ fvenv) t





mulEApp :: Exp -> [Exp] -> Exp
mulEApp _ [] = error "Application with no arguments!"
mulEApp fun args = foldl EApp fun args

tApp :: Type -> Type -> Type
tApp arg ret = TypeConstr "->" [arg, ret]

mulTApp :: [Type] -> Type -> Type
mulTApp args ret = foldr tApp ret args
