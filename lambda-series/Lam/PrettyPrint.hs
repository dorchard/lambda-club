{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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
    pprint (Sig e t) = pprint e ++ " : (" ++ pprint t ++ ")"
    pprint (Ext e) = pprint e

instance PrettyPrint PCF where
    isLexicallyAtomic _         = True

    pprint Zero     = "zero"
    pprint (Succ e) = "succ(" ++ pprint e ++ ")"
    pprint _        = error "Not implemented yet"

instance PrettyPrint () where
    pprint () = "()"

instance PrettyPrint Type where
    isLexicallyAtomic NatTy = True
    isLexicallyAtomic _     = False

    pprint NatTy = "Nat"
    pprint (FunTy tyA tyB) =
      "(" ++ pprint tyA ++ " -> " ++ pprint tyB ++ ")"