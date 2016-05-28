{-# LANGUAGE LambdaCase #-}

module Exceptions.EvalErrors where

data EvalError
    = NonExhaustivePatterns
    | LetPatternMismatch String
    | ListPatternLengthMismatch Int Int


instance Show EvalError where
    show = \case
        NonExhaustivePatterns -> "Non-exhaustive patterns in match!"
        LetPatternMismatch val -> "Pattern matching error; the value of " ++ val ++
            " does not match with given pattern."
        ListPatternLengthMismatch expected actual -> "no lol... (length mismatch)" ++ show expected ++ "vs. " ++ show actual -- TODO enhance this