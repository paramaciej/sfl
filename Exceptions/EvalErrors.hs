{-# LANGUAGE LambdaCase #-}

module Exceptions.EvalErrors where

data EvalError
    = NonExhaustivePatterns
    | ListPatternLengthMismatch Int Int


instance Show EvalError where
    show = \case
        NonExhaustivePatterns -> "Non-exhaustive patterns in match!"
        ListPatternLengthMismatch expected actual -> "no lol... (length mismatch)" ++ show expected ++ "vs. " ++ show actual