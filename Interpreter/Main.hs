import AbsSFL as SFL
import ParSFL
import ErrM
import TypeChecker.Types
import Control.Monad.State
import StdLib.Operators
import Data.Map
import Interpreter.Types
import Interpreter.Utils
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    stdTypes <- ops
    case args of
        [] -> do
            putStrLn "SFL -- Simple Functional Language\n (:q to quit)"
            _ <- runStateT userLines (PrEnv stdTypes eee)
            putStrLn "Goodbye."
        (filename:_) -> do
            _ <- runStateT (fromFile filename) (PrEnv stdTypes eee)
            return ()


fromFile :: String -> PrSt ()
fromFile filename = do
    program <- liftIO $ readFile filename
    case pProgram (myLexer program) of
        Ok (Prog stmts) -> mapM_ handleStmt stmts
        Bad str -> liftIO $ putStrLn $ "Error on parsing source file: " ++ str


userLines :: PrSt ()
userLines = do
        liftIO $ putStr "> " >> hFlush stdout
        line <- liftIO $ getLine
        case line of
            ":v" -> do
                vals <- gets values
                liftIO $ putStrLn $ show vals
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
            let ts = Forall [] t
            val <- evaluatedExp e
            put $ PrEnv (insert name ts typSt) (insert name val valSt)
            printExp e


