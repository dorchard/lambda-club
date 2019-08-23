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
  deriving Show

-- Top-level definitions
data Def ex =
  Def Id (Expr ex)
  deriving Show

-- Programs
type Program ex = [Def ex]

----------------------------
-- Arithmetic and conditional expressions

data Arith ex =
    BinOp String (Expr (Arith ex)) (Expr (Arith ex))
  | Conditional (Expr (Arith ex)) (Expr (Arith ex)) (Expr (Arith ex))
  | Constant Int
  deriving Show

data Hole = Hole