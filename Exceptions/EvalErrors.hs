{-# LANGUAGE LambdaCase #-}

module Exceptions.EvalErrors where

data EvalError
    = ZeroDivisionError
    | NonExhaustivePatterns
    | LetPatternMismatch String


instance Show EvalError where
    show = \case
        ZeroDivisionError -> "Divide by zero!"
        NonExhaustivePatterns -> "Non-exhaustive patterns in match!"
        LetPatternMismatch val -> "Pattern matching error; the value of " ++ val ++
            " does not match with given pattern."