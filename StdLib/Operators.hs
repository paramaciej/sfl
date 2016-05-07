module StdLib.Operators where

import TypeChecker.Types
import TypeChecker.HMUtils
import Data.Map

tInt :: Type
tInt = TypeConstr "Int" []

ops = fromList [
    ("plus", Forall [] $ mulTApp [tInt] (mulTApp [tInt] tInt)),
    ("succ", Forall [] $ mulTApp [tInt] tInt)]