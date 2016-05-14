{-# LANGUAGE LambdaCase #-}

module Exceptions.Types where

data TypeException
    = TypeMismatchError MismatchError
    | OccursCheckError String String
    | InferError String
    | UndefinedError String

data MismatchError = MismatchError String String (Maybe MismatchError)


instance Show TypeException where
    show = \case
        TypeMismatchError me -> "Type mismatch: " ++ show me
        OccursCheckError tv t -> "Occurs check failed: " ++ tv ++ " occurs in " ++ t ++ "."
        InferError s -> "Infer error: " ++ s
        UndefinedError name -> "Undefined '" ++ name ++ "'"


instance Show MismatchError where
--    show (MismatchError s s' mMerr) = s ++ " and " ++ s' ++ case mMerr of
--        Just merr -> "\n\t at " ++ show merr
--        Nothing -> ""

    show (MismatchError s s' mMerr) = (maybe "" show mMerr) ++ "\n\t at " ++ s ++ " and " ++ s'