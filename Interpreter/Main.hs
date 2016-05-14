import AbsSFL as SFL
import ParSFL
import ErrM
import TypeChecker.HindleyMilner
import TypeChecker.HMUtils
import TypeChecker.Types
import Control.Monad.State
import Control.Monad.Reader
import StdLib.Operators
import Data.Map
import Data.IORef
import Interpreter.Types
import Interpreter.Utils
import System.Environment


defState :: IO ProgramEnv
defState = do
    ref <- newIORef 12
    stdTypes <- runReaderT ops (Env ref empty)
    return $ PrEnv stdTypes eee

main :: IO ()
main = do
    args <- getArgs
    prEnv <- defState
    case args of
        [] -> do
            putStrLn "SFL -- Simple Functional Language\n (:q to quit)"
            _ <- runStateT userLines prEnv
            putStrLn "Goodbye."
        (filename:_) -> do
            runStateT (fromFile filename) prEnv
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
        line <- liftIO $ getLine
        case line of
            ":v" -> do
                vals <- gets values
                ttt <- gets types

                let aux = \(name, val) -> name ++ " -> " ++ show val
                liftIO $ putStrLn $ unlines $ Prelude.map aux $ assocs vals
                let aaa = \(name, t) -> do
                    xx <- showSchemeX t
                    liftIO $ putStrLn $ name ++ " ungeneralized: " ++ xx
                    tt <- instantiate t
                    txxx <- showScheme tt
                    liftIO $ putStrLn $ name ++ " of type " ++ txxx
                liftIO $ runReaderT (mapM_ aaa $ assocs (schemeMap ttt)) ttt
                userLines
            ":q" -> return ()
            _ -> do
                case pStmt (myLexer line) of
                    Ok stmt -> handleStmt stmt
                    _ -> liftIO $ putStrLn "Error on parsing line."
                userLines

handleStmt :: Stmt -> PrSt ()
handleStmt stmt = do
    case stmt of
        ExpStmt expStmt -> printExp expStmt

--        TypeDecl (UIdent name) idents tcs -> error "" --

        Function name arg args body -> do
            handleStmt (Value name (SFL.ELetRec name (lamCurry (arg:args) body) (SFL.ELiteral $ SFL.LVar name)))
          where
            lamCurry [a] b = SFL.ELam a b
            lamCurry (a:as) b = SFL.ELam a $ FBodyExp $ lamCurry as b
            lamCurry [] _ = error "impossible (function with no arguments)"

        Value (Ident name) e -> do
            PrEnv typSt valSt <- get
            t <- inferredType e
            ts <- liftIO $ runReaderT (generalize t) typSt
            sss <- liftIO $ runReaderT (showScheme t) typSt
            liftIO $ putStrLn $ "VALUE>>>>> " ++ sss
            sxxx <- liftIO $ runReaderT (showSchemeX ts) typSt
            liftIO $ putStrLn $ "VALUE<<<<<<<<< " ++ sxxx
            val <- evaluatedExp e
            put $ PrEnv (envInsert name ts typSt) (insert name val valSt)
            printExp e


