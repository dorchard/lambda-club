module Lam.PrettyPrint where

import Lam.Helpers
import Lam.Syntax

-- Pretty print terms
class PrettyPrint t where
    pprint :: t -> String

instance PrettyPrint (Expr ex) where
    pprint = todo "pprint expr"
