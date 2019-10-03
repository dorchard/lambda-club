module Lam.Semantics where

import Lam.Helpers
import Lam.Syntax

-- Small-step operational semantics reduction (full beta)
smallStep :: Expr t -> Expr t
smallStep = todo "Small steps"

-- `substitute t x t'` means t[t'/x]
substitute :: Expr t -> Id -> Expr t
substitute = todo "Substitution"

-- Keep doing small step reductions until normal form reached
multiStep :: Eq t => Expr t -> Expr t
multiStep t =
    let t' = smallStep t
    in  if t == t'
            -- Normal form reached
            then t'
            -- Can do more reduction
            else multiStep t'

