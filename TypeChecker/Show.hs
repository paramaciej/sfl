module TypeChecker.Show where

import TypeChecker.Types
import Control.Monad.Reader
import TypeChecker.Utils
import Data.Maybe

--showType :: Type -> IO String
--showType t = do
--    zonked <- zonk t
--    case zonked of
--        TypeConstr name ts -> do
--            tsStr <- mapM showType ts
--            return $ case name of
--                "->" -> case tsStr of
--                    [left, right] -> left ++ " -> " ++ right
--                    _ -> error "Wrong number of arguments in application!"
--                "list" -> case tsStr of
--                    [str] -> "[" ++ str ++ "]"
--                    _ -> error "Wrong number of arguments in list type!"
--                _ -> case tsStr of
--                    [] -> name
--                    _ -> "(" ++ name ++ concatMap (" " ++) tsStr ++ ")"
--        TypeVar tv -> return $ show tv


xxx :: Type -> (ReaderT [(TypeVar, String)] IO) String
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
            return $ fromMaybe "unknown" yyy

showType :: Type -> Tc String
showType t = do
    ts <- generalize t
    showScheme ts

showScheme :: TypeScheme -> Tc String
showScheme (Forall tvs tt) = liftIO $ runReaderT (xxx tt) (zip tvs (map (: []) ['a'..]))
