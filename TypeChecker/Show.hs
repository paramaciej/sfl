{-# LANGUAGE LambdaCase #-}
module TypeChecker.Show where

import TypeChecker.Types
import Control.Monad.Reader
import TypeChecker.Utils
import Data.Maybe

import System.Console.ANSI

surroundSGR :: [SGR] -> String -> String
surroundSGR sgrs str = setSGRCode sgrs ++ str ++ setSGRCode [Reset]

auxShowType :: Type -> (ReaderT [(TypeVar, String)] IO) String
auxShowType t = do
    liftIO (zonk t) >>= \case
        TypeConstr name ts -> do
            tsStr <- mapM auxShowType ts
            return $ showConstr name tsStr
          where
            showConstr = \case
                "->" -> \case
                   [left, right] -> left ++ " -> " ++ right
                   _ -> error "Wrong number of arguments in application!"
                "list" -> \case
                   [str] -> "[" ++ str ++ "]"
                   _ -> error "Wrong number of arguments in list type!"
                "Int" -> \case
                   [] -> surroundSGR [SetColor Foreground Vivid Magenta] "Int"
                   _ -> error "Wrong number of arguments for Int!"
                "Bool" -> \case
                    [] -> surroundSGR [SetColor Foreground Vivid Cyan] "Bool"
                    _ -> error "Wrong number of aeguments for Bool!"
                _ -> \case
                   [] -> name
                   args -> "(" ++ name ++ concatMap (" " ++) args ++ ")"
        TypeVar tv -> do
            yyy <- asks $ lookup tv
            return $ fromMaybe "unknown" yyy

showType :: Type -> Tc String
showType t = do
    ts <- generalize t
    showScheme ts

showScheme :: TypeScheme -> Tc String
showScheme (Forall tvs tt) = liftIO $ runReaderT (auxShowType tt) (zip tvs (map (: []) ['a'..]))
