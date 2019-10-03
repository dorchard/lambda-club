module Lam.Syntax where

import Data.Set

type Id = String

-- Abstract-syntax tree for LambdaCore
-- parameterised by an additional type `ex`
-- used to represent the abstract syntax
-- tree of additional commands
data Expr ex =
    Abs Id (Expr ex)
  | App (Expr ex) (Expr ex)
  | Var Id
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
-- Bound and free variables

bound_vars :: Expr ex -> Set Id
bound_vars (Abs var e) = var `insert` bound_vars e
bound_vars (App e1 e2) = bound_vars e1 `union` bound_vars e2
bound_vars (Var var) = singleton var

free_vars :: Expr ex -> Set Id
free_vars (Abs var e) = delete var $ free_vars e
free_vars (App e1 e2) = free_vars e1 `union` free_vars e2
free_vars (Var var) = singleton var

----------------------------
-- Fresh variable with respect to a set of variables
-- By adding apostrophes to a supplied initial variable

fresh_var :: Id -> Set Id -> Id
fresh_var var vars =
  if var `member` vars then fresh_var (var ++ "'") vars else var
