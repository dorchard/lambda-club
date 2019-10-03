module Lam.Syntax where

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
