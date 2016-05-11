import AbsSFL as SFL
import LexSFL
import ParSFL
import ErrM
import TypeChecker.HindleyMilner
import TypeChecker.Infer
import TypeChecker.HMUtils
import TypeChecker.Types
import Interpreter.Evaluator
import Control.Monad.State
import Control.Monad.Reader
import StdLib.Operators
import Data.Map
import Interpreter.Types
import Interpreter.Utils



main :: IO ()
main = do
    putStrLn "SFL -- Simple Functional Language\n (:q to quit)"
    stdTypes <- ops
    _ <- runStateT userLines (PrEnv stdTypes eee)
    putStrLn "Goodbye."



userLines :: PrSt ()
userLines = do
        liftIO $ putStr "> "
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

--        TypeDecl (UIdent name) idents tcs -> error "" -- TODO

        Function name arg args body -> do
            liftIO $ putStrLn $ show $ tcExp (lamCurry (arg:args) body)
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


