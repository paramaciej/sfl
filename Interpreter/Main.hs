{-# LANGUAGE LambdaCase #-}

import AbsSFL as SFL
import ParSFL
import ErrM
import TypeChecker.HindleyMilner
import TypeChecker.Utils
import TypeChecker.Show
import TypeChecker.Types
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import StdLib.Operators
import Data.Map
import Data.IORef
import Interpreter.Types
import Interpreter.Utils
import System.Environment
import System.IO


defState :: IO ProgramEnv
defState = do
    ref <- newIORef 0
    stdTypes <- runReaderT (runExceptT ops) (Env ref empty) >>= either fail return
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
            runStateT (runExceptT $ fromFile filename) prEnv
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
                    _ -> liftIO $ putStrLn "Error on parsing line."
                userLines

showValues :: PrSt ()
showValues = do
    st <- get
    let combined = intersectionWith (,) (values st) (schemeMap $ types st)
    let aux (name, (v, t)) = do
        tStr <- showScheme t
        liftIO $ putStrLn $ name ++ " = " ++ show v ++ " : " ++ tStr
    liftIO $ runReaderT (runExceptT (mapM_ aux $ assocs combined)) (types st) >>= either fail return


handleStmt :: Stmt -> PrSt ()
handleStmt = \case
        ExpStmt expStmt -> do
            x <- catchError (printExp expStmt) aux
            return x
          where
            aux str = liftIO $ hPutStrLn stderr str

--        TypeDecl (UIdent name) idents tcs -> error "" --

        Function name arg args body -> handleStmt
            (Value name (SFL.ELetRec name (lamCurry (arg:args) body) (SFL.ELiteral $ SFL.LVar name)))
          where
            lamCurry [a] b = SFL.ELam a b
            lamCurry (a:as) b = SFL.ELam a $ FBodyExp $ lamCurry as b
            lamCurry [] _ = error "impossible (function with no arguments)"

        Value (Ident name) e -> do
            PrEnv typSt valSt <- get
            t <- inferredType e
            ts <- liftIO $ runReaderT (runExceptT $ generalize t) typSt >>= either fail return
            val <- evaluatedExp e
            put $ PrEnv (envInsert name ts typSt) (insert name val valSt)
            printExp e


