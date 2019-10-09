module Lam.PrettyPrint where

import Lam.Syntax

-- Pretty print terms
class PrettyPrint t where
    isLexicallyAtomic :: t -> Bool
    isLexicallyAtomic _ = False

    pprint :: t -> String

bracket_pprint :: PrettyPrint t => t -> String
bracket_pprint t | isLexicallyAtomic t = pprint t
                 | otherwise           = "(" ++ pprint t ++ ")"

-- Untyped lambda calculus
instance PrettyPrint ex => PrettyPrint (Expr ex) where
    isLexicallyAtomic (Var _) = True
    isLexicallyAtomic _       = False

    pprint (Abs var e) = "\\" ++ var ++ " -> " ++ pprint e
    pprint (App (Abs var e1) e2) =
      bracket_pprint (Abs var e1) ++ " " ++ bracket_pprint e2
    pprint (App e1 e2) = pprint e1 ++ " " ++ bracket_pprint e2
    pprint (Var var) = var
    pprint (Ext e) = pprint e

instance PrettyPrint ex => PrettyPrint (Arith ex) where
    isLexicallyAtomic (Constant _) = True
    isLexicallyAtomic _         = False

    pprint (BinOp op e1 e2) = pprint e1 ++ " " ++ op ++ " " ++ pprint e2
    pprint (Conditional e1 e2 e3) = "if " ++ pprint e1 ++ " then " ++ pprint e2 ++ " else " ++ pprint e3
    pprint (Constant n) = show n

instance PrettyPrint () where
    pprint () = "()"
