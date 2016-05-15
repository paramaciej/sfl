{-# LANGUAGE LambdaCase #-}

module Exceptions.Types where

data TypeException
    = TypeMismatchError MismatchError
    | OccursCheckError String String
    | ApplicationTypeError String String TypeException
    | InferError String TypeException
    | UndefinedError String
    | EmptyMatchError
    | DifferentCaseTypesError TypeException
    | PatternMatchingError String String TypeException
    | ConstructorArgsNumber String Int Int

data MismatchError = MismatchError String String (Maybe MismatchError)


instance Show TypeException where
    show = \case
        TypeMismatchError me -> "Type mismatch: " ++ show me
        OccursCheckError tv t -> "Occurs check failed: " ++ tv ++ " occurs in " ++ t ++ "."
        ApplicationTypeError funStr argStr err ->
            "The application of the function of type: " ++ funStr ++ "\n" ++
            "                to the argument of type: " ++ argStr ++ "\n" ++ show err
        InferError s exc -> "Infer error: " ++ s ++ "\n" ++ show exc
        UndefinedError name -> "Undefined '" ++ name ++ "'"
        EmptyMatchError -> "No cases in match expression."
        DifferentCaseTypesError err -> "Cases at match expression have different return types.\n" ++ show err
        PatternMatchingError patStr tStr err ->
            "Wrong types at matching patern: " ++ patStr ++ "\n" ++
            "       with expression of type: " ++ tStr ++ "\n" ++ show err
        ConstructorArgsNumber constrName rightNumber wrongNumber ->
            "Type constructor '" ++ constrName ++ "' requires " ++ show rightNumber ++ " arguments " ++
            "(" ++ show wrongNumber ++ " was given)."


instance Show MismatchError where
    show (MismatchError s s' mMerr) = (maybe "" show mMerr) ++ "\n  at " ++ s ++ " and " ++ s'