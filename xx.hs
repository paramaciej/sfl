import AbsSFL
import LexSFL
import ParSFL
import ErrM
import TypeChecker.HindleyMilner
import TypeChecker.Infer
import TypeChecker.HMUtils
import Interpreter.Evaluator
import Control.Monad.Reader
import StdLib.Operators

main :: IO ()
main = do
    putStrLn "Welcome!"
    x <- getContents
    mapM_ aux (lines x) where
      aux :: String -> IO ()
      aux s = case pStmt (myLexer s) of
        Ok stmt -> do
            putStrLn $ "Sparsowano: " ++ show stmt
            case stmt of
                ExpStmt ss -> do
                    putStrLn $ "tcExp: " ++ show (tcExp ss)
                    t <- inferredType ss
                    tStr <- showType t
                    putStrLn $ "Typ: " ++ tStr

                    value <- runReaderT (eval (tcExp ss)) eee
                    putStrLn $ "Eval: " ++ show value
                _ -> putStrLn "nieznane wyrażenie"
        _ -> putStrLn "no elo błąd"

