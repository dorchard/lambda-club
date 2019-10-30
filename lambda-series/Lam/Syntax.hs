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
    Case (Expr PCF) (Expr PCF) (Identifier, Expr PCF)
                     -- case e of zero -> e1; succ x -> e2
  | Fix (Expr PCF)   -- fix(e)
  | Succ (Expr PCF)  -- succ(e)
  | Zero             -- zero
  deriving Show

------------------------------
-- Type syntax

data Type =
    FunTy Type Type  -- A -> B
  | NatTy            -- Nat
  deriving (Show, Eq)

----------------------------
-- Bound and free variables (doesn't work over Ext terms yet)

bound_vars :: Expr ex -> Set.Set Identifier
bound_vars (Abs var e) = var `Set.insert` bound_vars e
bound_vars (App e1 e2) = bound_vars e1 `Set.union` bound_vars e2
bound_vars (Var var)   = Set.singleton var
bound_vars (Sig e _)   = bound_vars e
bound_vars (Ext _)     = Set.empty

free_vars :: Expr ex -> Set.Set Identifier
free_vars (Abs var e) = Set.delete var (free_vars e)
free_vars (App e1 e2) = free_vars e1 `Set.union` free_vars e2
free_vars (Var var)   = Set.singleton var
free_vars (Sig e _)   = free_vars e
free_vars (Ext _)     = Set.empty

----------------------------
-- Fresh variable with respect to a set of variables
-- By adding apostrophes to a supplied initial variable

fresh_var :: Identifier -> Set.Set Identifier -> Identifier
fresh_var var vars =
  if var `Set.member` vars then fresh_var (var ++ "'") vars else var

------------------------------
-- Language options that `lcore` accepts in files

data Option = PCF | Typed
  deriving Eq

-- Some helpers

isPCF :: [Option] -> Bool
isPCF options = elem PCF options

isTyped :: [Option] -> Bool
isTyped options = elem Typed options

language :: [Option] -> String
language options = if isPCF options then "PCF" else "lambda"