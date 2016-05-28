{-# LANGUAGE LambdaCase #-}

module TypeChecker.HindleyMilner where

import Data.IORef
import Control.Monad.Reader
import qualified Data.Map as M
import qualified AbsSFL as SFL
import TypeChecker.Types
import TypeChecker.Show
import TypeChecker.Utils
import Control.Monad.Except
import Exceptions.TypeErrors
import Exceptions.Utils

infer :: Exp -> Tc Type
infer (EVar x) = do
    mts <- asks $ M.lookup x . schemeMap
    case mts of
        Just ts -> instantiate ts
        Nothing -> throwError $ UndefinedError x

infer (EApp e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    b <- fresh
    let tb = TypeVar b
    catchAppInfer (unify t1 (TypeConstr "->" [t2, tb])) t1 t2
    return tb

infer (ELam x e) = do
    b <- fresh
    let tb = TypeVar b
    t <- local (envInsert x (Forall [] tb)) $ infer e
    return $ TypeConstr "->" [tb, t]

infer (ELet patExp e body) = do
    t <- infer e
    modifications <- inferPatExp patExp t
    local modifications $ infer body

infer (ELetRec name e body) = do
    modifier <- letRecEnvModifier name e
    local modifier $ infer body

infer (EInt _) = return tInt

infer (EBool _) = return tBool

infer (EIf cond eTrue eFalse) =
    catchIfInfer (infer (mulEApp (EVar "_infer_if") [cond, eTrue, eFalse]))

infer (EMatch e cases) = do
    t <- infer e
    let aux (patExp, caseBody) = do
        mods <- inferPatExp patExp t
        local mods $ infer caseBody

    modifiers <- mapM aux cases
    case modifiers of
        (m:ms) -> do
            ss <- showType m
            sss <- mapM showType ms

            liftIO $ putStrLn $ ss ++ "vs.\n\t" ++ show sss
            mapM_ (catchMatchInfer . unify m) ms
            return m
        [] -> throwError EmptyMatchError

infer (EConstr name) = do
    constructor <- asks (M.lookup name . typeConstrs)
    case constructor of
        Just constr -> instantiate $ constrType constr
        Nothing -> throwError $ UndefinedError $ "constructor: " ++ name

infer (ETuple es) = do
    ts <- mapM infer es
    return $ TypeConstr "tuple" ts


inferPatExp :: SFL.PatExp -> Type -> Tc (Env -> Env)
inferPatExp patExp ttt = do
    zonked <- liftIO $ zonk ttt
    case patExp of
        SFL.PETuple pe1 pe2 -> do
            free1 <- fresh
            free2 <- fresh
            catchPatternMatch (unify (TypeConstr "tuple" [TypeVar free1, TypeVar free2]) zonked) patExp zonked
            liftIO (zonk zonked) >>= \case
                TypeConstr "tuple" [t1, t2] -> do
                    z1 <- inferPatExp pe1 t1
                    z2 <- inferPatExp pe2 t2
                    return (z1 . z2)
                _ -> error "wrong tuple matching"
        SFL.PECons pe1 pe2 -> do
            free<- fresh
            catchPatternMatch (unify (TypeConstr "list" [TypeVar free]) zonked) patExp zonked
            liftIO (zonk zonked) >>= \case
                TypeConstr "list" [lt] -> do
                    z1 <- inferPatExp pe1 lt
                    z2 <- inferPatExp pe2 $ TypeConstr "list" [lt]
                    return (z1 . z2)
                _ -> error "wrong cons matching"
        SFL.PEPat (SFL.PatVar (SFL.Ident name)) -> do
            ts <- generalize zonked
            return  $ envInsert name ts
        SFL.PEPat (SFL.PatTConstr (SFL.UIdent name) pats) -> do
            constructor <- asks (M.lookup name . typeConstrs)
            case constructor of
                Just constr -> do
                    let constrTypeName = typeName constr
                    cType <- getUserType constrTypeName
                    unify cType zonked
                    case zonked of
                        TypeConstr n args -> if n == constrTypeName
                            then do
                                zs <- zipWithM inferPatExp pats args
                                return $ foldr (.) id zs
                            else throwError $ ConstructorsMismatch n constrTypeName
                        _ -> error "wrong type constructor"
                Nothing -> throwError $ UndefinedError name

        SFL.PEPat SFL.PatWild -> return id
        SFL.PEPat SFL.PatTrue -> unify tBool zonked >> return id
        SFL.PEPat SFL.PatFalse -> unify tBool zonked >> return id
        SFL.PEPat (SFL.PatInt _) -> unify tInt zonked >> return id
        SFL.PEPat (SFL.PatList elems) -> do
                free <- fresh
                catchPatternMatch (unify (TypeConstr "list" [TypeVar free]) zonked) patExp zonked
                liftIO (zonk zonked) >>= \case
                    TypeConstr "list" [lt] -> do
                        let patExps = map (\(SFL.PatLElem p) -> p) elems
                        zs <- mapM (`inferPatExp` lt) patExps
                        return $ foldr (.) id zs
                    _ -> error "wrong list matching"

instantiate :: TypeScheme -> Tc Type
instantiate (Forall tvs t) = do
    tvs' <- mapM (const fresh) tvs
    let subst = zip tvs tvs'
    liftIO $ liftM (applySubstr subst) (zonk t)

unify :: Type -> Type -> Tc ()
unify (TypeVar tv) t' = unifyVar tv t' False
unify t (TypeVar tv') = unifyVar tv' t True
unify t@(TypeConstr name args) t'@(TypeConstr name' args')
    | name == name' = catchError (zipWithM_ unify args args') (raiseMismatchError t t' . Just)
    | otherwise = raiseMismatchError t t' Nothing

unifyVar :: TypeVar -> Type -> Bool -> Tc ()
unifyVar tv@(TV _ ioref) t reversed = liftIO (readIORef ioref) >>= \case
        Just context -> if reversed
            then unify t context
            else unify context t
        Nothing -> do
            zonked <- liftIO $ zonk t
            if occursCheck tv zonked then case zonked of
                TypeVar _ -> return () -- type is the same typevar -- it's already unified.
                _ -> raiseOccursCheckError tv zonked
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