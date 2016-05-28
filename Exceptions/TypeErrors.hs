{-# LANGUAGE LambdaCase #-}

module Exceptions.TypeErrors where

data TypeError
    = TypeMismatchError MismatchError
    | OccursCheckError String String
    | ApplicationTypeError String String TypeError
    | InferError String TypeError
    | UndefinedError String
    | EmptyMatchError
    | DifferentCaseTypesError TypeError
    | PatternMatchingError String String TypeError
    | ConstructorArgsNumber String Int Int
    | TypeArgsNumber String Int Int
    | TypeDeclared String
    | ConstrDeclared String
    | DuplicatedConstructors

data MismatchError = MismatchError String String (Maybe MismatchError)


instance Show TypeError where
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
        TypeArgsNumber tName rightNumber wrongNumber ->
            "Type '" ++ tName ++ "' requires " ++ show rightNumber ++ " parameters " ++
            "(" ++ show wrongNumber ++ " was given)."
        TypeDeclared tName -> "The type '" ++ tName ++ "' has been already declared."
        ConstrDeclared cName -> "The type constructor '" ++ cName ++ "' has been already declared."
        DuplicatedConstructors -> "Trying to declare type with duplicated names of constructors!"



instance Show MismatchError where
    show (MismatchError s s' mMerr) = maybe "" show mMerr ++ "\n  at " ++ s ++ " and " ++ s'