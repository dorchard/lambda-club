module Lam.Semantics where

import Lam.Syntax

import qualified Data.Set as Set

-- Small-step operational semantics reduction (full beta)
smallStep :: Expr t -> Maybe (Expr t)
smallStep (Var _) = Nothing
-- Beta reduction
smallStep (App (Abs x e) e') =
  Just (substitute e (x, e'))
-- App rules
smallStep (App e1 e2) =
  case smallStep e1 of
    -- App L rule
    Just e1' -> Just (App e1' e2)
    Nothing ->
      case smallStep e2 of
        Nothing -> Nothing
        -- App R rule
        Just e2' -> Just (App e1 e2')
-- Abs rule
smallStep (Abs x e) =
  case smallStep e of
    Just e' -> Just (Abs x e')
    Nothing -> Nothing

smallStep (Sig e _) = smallStep e
smallStep (Ext _) = Nothing

-- `substitute e (x, e')` means e[e'/x]
substitute :: Expr t -> (Identifier, Expr t) -> Expr t
substitute (Var y) (x, e')
  | x == y = e'
  | otherwise = Var y

substitute (App e1 e2) (x, e') =
  App (substitute e1 (x, e')) (substitute e2 (x, e'))

substitute (Abs y e) (x, e')
  -- Name clash in lambda abstraction
  | x == y = Abs y e
  -- If e' contains y (then we have to be super careful)
  | y `Set.member` free_vars e' =
    let y' = fresh_var y (free_vars e' `Set.union` free_vars e)
    in Abs y' (substitute (substitute e (y, Var y')) (x, e'))

  -- Example: substitute (\y -> x y) (x, y y)
  --           = (\y' -> (y y) y')

  -- No name clash
  | otherwise = Abs y (substitute e (x, e'))

substitute (Sig e t) s = Sig (substitute e s) t

-- Ext case we are ignoring
substitute (Ext e) _ = Ext e

-- Keep doing small step reductions until normal form reached
multiStep :: Expr t -> (Expr t, Int)
multiStep e = multiStep' e 0

multiStep' :: Expr t -> Int -> (Expr t, Int)
multiStep' t n =
    case smallStep t of
      -- Normal form reached
      Nothing -> (t, n)
      -- Can do more reduction
      Just t' -> multiStep' t' (n+1)

