{-# LANGUAGE LambdaCase #-}

import AbsSFL as SFL
import ParSFL
import ErrM
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Map
import Interpreter.Types
import Interpreter.Utils
import StdLib.Operators
import System.Environment
import System.IO
import TypeChecker.Show
import TypeChecker.Types
import TypeChecker.Utils
import UserTypes.Declare
import UserTypes.Vals


defState :: IO ProgramEnv
defState = do
    ref <- newIORef 0
    stdTypes <- runReaderT (runExceptT ops) (Env ref empty empty empty) >>= either (fail . show) return
    return $ PrEnv stdTypes eee

main :: IO ()
main = do
    args <- getArgs
    prEnv <- defState
    case args of
        [] -> do
            putStrLn "SFL -- Simple Functional Language\n (:q to quit)"
            _ <- runStateT (runExceptT userLines) prEnv
            putStrLn "Goodbye."
        (filename:_) -> do
            _ <- runStateT (runExceptT $ fromFile filename) prEnv
            return ()


fromFile :: String -> PrSt ()
fromFile filename = do
    program <- liftIO $ readFile filename
    case pProgram (myLexer program) of
        Ok (Prog stmts) -> mapM_ handleStmt stmts
        Bad str -> liftIO $ putStrLn $ "Error on parsing source file: " ++ str


userLines :: PrSt ()
userLines = do
        liftIO $ putStr "> "
        line <- liftIO getLine
        case line of
            ":v" -> do
                showValues
                userLines
            ":q" -> return ()
            _ -> do
                case pStmt (myLexer line) of
                    Ok stmt -> handleStmt stmt
                    Bad x -> liftIO $ putStrLn $ "Error on parsing line: " ++ show x
                userLines

showValues :: PrSt ()
showValues = do
    st <- get
    let combined = intersectionWith (,) (values st) (schemeMap $ types st)
    let aux (name, (v, t)) = do
        tStr <- showScheme t
        liftIO $ putStrLn $ name ++ " \t= " ++ show v ++ " : " ++ tStr
    liftIO $ runReaderT (runExceptT (mapM_ aux $ assocs combined)) (types st) >>= either (fail .show) return


handleStmt :: Stmt -> PrSt ()
handleStmt = \case
        ExpStmt expStmt -> catchError (printExp expStmt) (liftIO . hPutStrLn stderr)

        tDecl@(TypeDecl _ _ _) -> do
            catchError (handleDecl) (liftIO . hPutStrLn stderr)
          where
            handleDecl = do
                PrEnv typeSt valSt <- get
                modTypeSt <- liftIO (runReaderT (runExceptT $ declareType tDecl) typeSt)
                    >>= either (throwError . show) return
                modValSt <- liftIO (runReaderT (runExceptT $ defineConstrs tDecl) valSt)
                    >>= either (throwError . show) return
                put $ PrEnv (modTypeSt typeSt) (modValSt valSt)

        Function name arg args body -> handleStmt
            (Value name (SFL.ELetRec name (lamCurry (arg:args) body) (SFL.ELiteral $ SFL.LVar name)))
          where
            lamCurry [a] b = SFL.ELam a b
            lamCurry (a:as) b = SFL.ELam a $ FBodyExp $ lamCurry as b
            lamCurry [] _ = error "impossible (function with no arguments)"

        Value (Ident name) e -> do
            catchError (handleValue) (liftIO . hPutStrLn stderr)
          where
            handleValue = do
                PrEnv typSt valSt <- get
                t <- inferredType e
                ts <- liftIO $ runReaderT (runExceptT $ generalize t) typSt >>= either (fail . show) return
                val <- evaluatedExp e
                put $ PrEnv (envInsert name ts typSt) (insert name val valSt)
                printExp e


