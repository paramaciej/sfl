{-# LANGUAGE LambdaCase #-}

module Exceptions.EvalErrors where

data EvalError
    = NonExhaustivePatterns
    | LetPatternMismatch String


instance Show EvalError where
    show = \case
        NonExhaustivePatterns -> "Non-exhaustive patterns in match!"
        LetPatternMismatch val -> "Pattern matching error; the value of " ++ val ++
            " does not match with given pattern."