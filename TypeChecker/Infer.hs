module TypeChecker.Infer where

import qualified AbsSFL as SFL
import TypeChecker.Types

tcExp :: SFL.Exp -> Exp
tcExp