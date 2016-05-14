{-# LANGUAGE LambdaCase #-}

module TypeChecker.HindleyMilner where

import Data.IORef
import Data.List
import Control.Monad.Reader
import qualified Data.Map as M
import qualified AbsSFL as SFL
import TypeChecker.Types
import TypeChecker.FTV
import TypeChecker.HMUtils

infer :: Exp -> Tc Type
infer (EVar x) = do
    mts <- asks $ (M.lookup x) . schemeMap
    case mts of
        Just ts -> instantiate ts
        Nothing -> error $ "'" ++ x ++ "' undefined." -- FIXME

infer (EApp e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    b <- fresh
    let tb = TypeVar b


    tstr1 <- showScheme t1
    tstr2 <- showScheme t2
    tbstr <- showScheme tb
    liftIO $ putStrLn $ "APP BefUnify " ++ tstr1 ++ " | " ++ tstr2 ++ " ====> " ++ tbstr

    unify t1 (TypeConstr "->" [t2, tb])

    tstr1 <- showScheme t1
    tstr2 <- showScheme t2
    tbstr <- showScheme tb
    liftIO $ putStrLn $ "APP AftUnify " ++ tstr1 ++ " | " ++ tstr2 ++ " ====> " ++ tbstr
    return tb

infer (ELam x e) = do
    b <- fresh
    let tb = TypeVar b
    t <- local (envInsert x (Forall [] tb)) $ infer e
    se <- showScheme t
    stb <- showScheme tb
    liftIO $ putStrLn $ "ELAM! " ++ "from: " ++ stb ++ ", to: " ++ se
    return $ TypeConstr "->" [tb, t]

infer (ELet patExp e body) = do
    t <- infer e
    tstr <- showScheme t
    liftIO $ putStrLn $ "ELET: " ++ tstr
    modifications <- inferPatExp patExp t
    local modifications $ infer body

infer (ELetRec name e body) = do
    modifier <- letRecEnvModifier name e
    local modifier $ infer body


infer (EInt _) = return tInt

infer (EBool _) = return tBool

infer (EIf cond eTrue eFalse) = infer (mulEApp (EVar "_infer_if") [cond, eTrue, eFalse])

infer (EMatch e cases) = do
    t <- infer e
    let aaa = \(patExp, caseBody) -> do
        mods <- inferPatExp patExp t
        local mods $ infer caseBody

    modifiers <- mapM aaa cases
    case modifiers of
        (m:ms) -> do
            mapM_ (unify m) ms
            return m
        [] -> error "no cases in match!" -- FIXME

infer (EConstr name es) = do
    x <- mapM infer es
    return $ TypeConstr name x


inferPatExp :: SFL.PatExp -> Type -> Tc (Env -> Env)
inferPatExp patExp ttt = do
    zonked <- liftIO $ zonk ttt
    case patExp of
        SFL.PETuple pe1 pe2 -> case zonked of
            TypeConstr "tuple" [t1, t2] -> do
                z1 <- inferPatExp pe1 t1
                z2 <- inferPatExp pe2 t2
                return (z1 . z2)
            _ -> error "wrong tuple matching" -- FIXME
        SFL.PECons pe1 pe2 -> do
            liftIO $ putStrLn $ "no elo: " ++ show pe1 ++ "/" ++ show pe2
            xx <- liftIO $ showType zonked
            liftIO $ putStrLn $ "typ: " ++ xx
            bbb <- fresh
            let tbbb = TypeVar bbb
            unify (TypeConstr "list" [tbbb]) zonked
            xxxx <- liftIO $ showType zonked
            liftIO $ putStrLn $ "after unify: " ++ xxxx
            superZonk <- liftIO $ zonk zonked
            case superZonk of
                TypeConstr "list" [lt] -> do
                    z1 <- inferPatExp pe1 lt
                    z2 <- inferPatExp pe2 $ TypeConstr "list" [lt]
                    return (z1 . z2)
                TypeConstr x y -> do
                    error $ "XXYssssXX: " ++ x
                _ -> error $ "wrong cons matching:" ++ xxxx -- FIXME
        SFL.PEPat (SFL.PatVar (SFL.Ident name)) -> do
            ts <- generalize zonked
            return  $ envInsert name ts
        SFL.PEPat (SFL.PatTConstr (SFL.UIdent name) pats) -> do
            case zonked of
                TypeConstr n args -> if n == name
                    then do
                        zs <- mapM (\(p, a) -> inferPatExp p a) (zip pats args)
                        let composed = comp zs where
                            comp [] = id
                            comp (h:t) = h . (comp t)
                        return  composed
                    else error "constructor names mismatch!" -- FIXME
                _ -> error "wrong type constr."
        SFL.PEPat SFL.PatWild -> return id
        SFL.PEPat SFL.PatTrue -> return id
        SFL.PEPat SFL.PatFalse -> return id
        SFL.PEPat (SFL.PatInt _) -> return id


instantiate :: TypeScheme -> Tc Type
instantiate (Forall tvs t) = do
    tvs' <- mapM (const fresh) tvs
    let subst = zip tvs tvs'
    zonked <- liftIO $ zonk t
    return $ applySubstr subst zonked

unify :: Type -> Type -> Tc ()
unify (TypeVar tv) t' = unifyVar tv t'
unify t (TypeVar tv') = unifyVar tv' t
unify (TypeConstr name args) (TypeConstr name' args')
    | name == name' = zipWithM_ unify args args'
    | otherwise = fail $ "Type mismatch: " ++ name ++ " vs. " ++ name'

unifyVar :: TypeVar -> Type -> Tc ()
unifyVar tv@(TV _ ioref) t = liftIO (readIORef ioref) >>= \case
        Just context -> unify context t
        Nothing -> do
            zonked <- liftIO $ zonk t
            if occursCheck tv zonked then
                fail "occurs check failed"
            else
                liftIO $ writeIORef ioref (Just zonked)


letRecEnvModifier :: String -> Exp -> Tc (Env -> Env)
letRecEnvModifier name e = do
    rm <- recMod
    t <- local rm $ infer e
    ts <- generalize t
    return $ envInsert name ts
  where
    recMod = do
        fr <- fresh
        return $ envInsert name (Forall [] $ TypeVar fr)