{-# LANGUAGE GADTs #-}

module Lam.Syntax where

import qualified Data.Set as Set

type Identifier = String

-- Abstract-syntax tree for LambdaCore
-- parameterised by an additional type `ex`
-- used to represent the abstract syntax
-- tree of additional commands
data Expr ex where
    Abs :: Identifier -> Expr ex -> Expr ex -- \x -> e  [λ x . e]
    App :: Expr ex ->  Expr ex   -> Expr ex -- e1 e2
    Var :: Identifier            -> Expr ex -- x

    Sig :: Expr ex -> Type       -> Expr ex -- e : A

    -- Extend the ast at this point
    Ext :: ex -> Expr ex
  deriving Show

----------------------------
-- Extend the language to PCF (natural number constructors
-- and deconstructor + fixed point)

data PCF =
    NatCase (Expr PCF) (Expr PCF) (Identifier, Expr PCF)
                               -- case e of zero -> e1 | succ x -> e2
  | Fix (Expr PCF)             -- fix(e)
  | Succ                       -- succ (function)
  | Zero                       -- zero
  | Pair (Expr PCF) (Expr PCF) -- <e1, e2>
  | Fst (Expr PCF)             -- fst(e)
  | Snd (Expr PCF)             -- snd(e)
  | Inl (Expr PCF)             -- inl(e)
  | Inr (Expr PCF)             -- inr(e)
  | Case (Expr PCF) (Identifier, Expr PCF) (Identifier, Expr PCF)
                               -- case e of inl x -> e1 | inr y -> e2
  deriving Show

isValue :: Expr PCF -> Bool
isValue (Abs _ _) = True
isValue (Var _)   = True
isValue e         = isNatVal e

isNatVal :: Expr PCF -> Bool
isNatVal (Ext Zero)  = True
isNatVal (Ext Succ)  = True
isNatVal (App e1 e2) = isNatVal e1 && isNatVal e2
isNatVal _           = False

------------------------------
-- Type syntax

data Type =
    FunTy Type Type  -- A -> B
  | NatTy            -- Nat
  | ProdTy Type Type -- A * B
  | SumTy Type Type  -- A + B
  deriving (Show, Eq)

----------------------------
-- Bound and free variables (doesn't work over Ext terms yet)

bound_vars :: Expr PCF -> Set.Set Identifier
bound_vars (Abs var e)                  = var `Set.insert` bound_vars e
bound_vars (App e1 e2)                  = bound_vars e1 `Set.union` bound_vars e2
bound_vars (Var var)                    = Set.singleton var
bound_vars (Sig e _)                    = bound_vars e
bound_vars (Ext (NatCase e e1 (x,e2)))  =
  x `Set.insert` (bound_vars e `Set.union` bound_vars e1 `Set.union` bound_vars e2)
bound_vars (Ext (Fix e))                = bound_vars e
bound_vars (Ext (Pair e1 e2))           = bound_vars e1 `Set.union` bound_vars e2
bound_vars (Ext (Fst e))                = bound_vars e
bound_vars (Ext (Snd e))                = bound_vars e
bound_vars (Ext (Inl e))                = bound_vars e
bound_vars (Ext (Inr e))                = bound_vars e
bound_vars (Ext (Case e (x,e1) (y,e2))) =
  bound_vars e `Set.union` (x `Set.insert` bound_vars e1) `Set.union` (y `Set.insert` bound_vars e2)
bound_vars (Ext _)                      = Set.empty

free_vars :: Expr PCF -> Set.Set Identifier
free_vars (Abs var e)                   = Set.delete var (free_vars e)
free_vars (App e1 e2)                   = free_vars e1 `Set.union` free_vars e2
free_vars (Var var)                     = Set.singleton var
free_vars (Sig e _)                     = free_vars e
free_vars (Ext (NatCase e e1 (x,e2)))   =
  free_vars e `Set.union` free_vars e1 `Set.union` (Set.delete x (free_vars e2))
free_vars (Ext (Fix e))                 = free_vars e
free_vars (Ext (Pair e1 e2))            = free_vars e1 `Set.union` free_vars e2
free_vars (Ext (Fst e))                 = free_vars e
free_vars (Ext (Snd e))                 = free_vars e
free_vars (Ext (Inl e))                 = free_vars e
free_vars (Ext (Inr e))                 = free_vars e
free_vars (Ext (Case e (x,e1) (y,e2)))  =
  free_vars e `Set.union` (Set.delete x (free_vars e1)) `Set.union` (Set.delete y (free_vars e2))
free_vars (Ext _)                       = Set.empty

----------------------------
-- Fresh variable with respect to a set of variables
-- By adding apostrophes to a supplied initial variable

fresh_var :: Identifier -> Set.Set Identifier -> Identifier
fresh_var var vars =
  if var `Set.member` vars then fresh_var (var ++ "'") vars else var