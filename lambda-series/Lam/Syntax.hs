module Lam.Syntax where

import qualified Data.Set as Set

type Id = String

-- Abstract-syntax tree for LambdaCore
-- parameterised by an additional type `ex`
-- used to represent the abstract syntax
-- tree of additional commands
data Expr ex =
    Abs Id (Expr ex)         -- \x -> e  [λ x . e]
  | App (Expr ex) (Expr ex)  -- e1 e2
  | Var Id                   -- x

  -- Extend the ast at this point
  | Ext ex
  deriving (Show, Eq)

----------------------------
-- Extend the language with arithmetic and conditional expressions
-- (For later)

data Arith ex =
    BinOp String (Expr (Arith ex)) (Expr (Arith ex))
  | Conditional (Expr (Arith ex)) (Expr (Arith ex)) (Expr (Arith ex))
  | Constant Int
  deriving (Show, Eq)

----------------------------
-- Bound and free variables (doesn't work over Ext terms yet)

bound_vars :: Expr ex -> Set.Set Id
bound_vars (Abs var e) = var `Set.insert` bound_vars e
bound_vars (App e1 e2) = bound_vars e1 `Set.union` bound_vars e2
bound_vars (Var var)   = Set.singleton var
bound_vars (Ext _)     = Set.empty

free_vars :: Expr ex -> Set.Set Id
free_vars (Abs var e) = Set.delete var (free_vars e)
free_vars (App e1 e2) = free_vars e1 `Set.union` free_vars e2
free_vars (Var var)   = Set.singleton var
free_vars (Ext _)     = Set.empty

----------------------------
-- Fresh variable with respect to a set of variables
-- By adding apostrophes to a supplied initial variable

fresh_var :: Id -> Set.Set Id -> Id
fresh_var var vars =
  if var `Set.member` vars then fresh_var (var ++ "'") vars else var
