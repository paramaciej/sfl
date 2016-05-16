{-# LANGUAGE LambdaCase #-}
module TypeChecker.Show where

import TypeChecker.Types
import Control.Monad.Reader
import TypeChecker.Utils
import Data.Maybe
import Data.List
import Interpreter.Show

import System.Console.ANSI

standardConstrs = ["->", "tuple", "list", "Int", "Bool"]

auxShowType :: Type -> (ReaderT [(TypeVar, String)] IO) String
auxShowType t = liftIO (zonk t) >>= \case
        TypeConstr name ts -> do
            zonkedTs <- mapM (liftIO . zonk) ts
            showConstr name zonkedTs
          where
            showConstr = \case
                "->" -> \case
                    [left, right] -> do
                        leftStr <- auxShowType left
                        rightStr <- auxShowType right
                        return $ (case left of
                            TypeConstr "->" _ -> "(" ++ leftStr ++ ")"
                            _ -> leftStr)
                            ++ surroundSGR [SetColor Foreground Vivid Yellow] " -> " ++ rightStr
                    _ -> error "Wrong number of arguments in application!"
                "tuple" -> \case
                    args -> do
                        argStrs <- mapM auxShowType args
                        let blue = surroundSGR [SetColor Foreground Vivid Blue]
                        return $ blue "(" ++ intercalate (blue ", ") argStrs ++ blue ")"

                "list" -> \case
                    [t'] -> do
                        tStr <- auxShowType t'
                        return $ "[" ++ tStr ++ "]"
                    _ -> error "Wrong number of arguments in list type!"
                "Int" -> \case
                    [] -> return $ surroundSGR [SetColor Foreground Vivid Magenta] "Int"
                    _ -> error "Wrong number of arguments for Int!"
                "Bool" -> \case
                    [] -> return $ surroundSGR [SetColor Foreground Vivid Cyan] "Bool"
                    _ -> error "Wrong number of aeguments for Bool!"
                _ -> \case
                    args -> do
                        argStrs <- mapM xxx args
                        return $ surroundSGR [SetColor Foreground Vivid Green] name ++ concatMap (" " ++) argStrs
                      where
                        xxx tt = case tt of
                            TypeConstr "->" _ -> surround tt
                            TypeConstr "tuple" _ -> auxShowType tt
                            TypeConstr "list" _ -> auxShowType tt
                            TypeConstr _ [] -> auxShowType tt
                            TypeConstr _ _ -> surround tt
                            TypeVar _ -> auxShowType tt
                        surround tt = do
                            tStr <- auxShowType tt
                            return $ "(" ++ tStr ++ ")"
        TypeVar tv -> do
            t' <- asks $ lookup tv
            return $ fromMaybe (show tv) t'

showType :: Type -> Tc String
showType t = do
    ts <- generalize t
    showScheme ts

showScheme :: TypeScheme -> Tc String
showScheme (Forall tvs tt) = liftIO $ runReaderT (auxShowType tt) (zip tvs (map (: []) ['a'..]))
