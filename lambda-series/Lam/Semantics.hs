module Lam.Semantics where

import Lam.Syntax

import qualified Data.Set as Set

-- Small-step operational semantics reduction
smallStep :: Expr PCF -> Maybe (Expr PCF)
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

smallStep (Sig e _) = Just e

-- Small-step for PCF terms
smallStep (Ext (Fix e)) =
  case smallStep e of
    Just e' -> Just (Ext $ Fix e')
    Nothing -> Just $ App e (Ext $ Fix e)

smallStep (Ext (Case (Ext Zero) e1 _)) = Just e1

smallStep (Ext (Case (App (Ext Succ) n) _ (x,e2))) = Just $ substitute e2 (x,n)

smallStep (Ext (Case e e1 (x,e2))) =
  case smallStep e of
    Just e' -> Just (Ext (Case e' e1 (x,e2)))
    Nothing -> Nothing

-- other Ext terms
smallStep (Ext _) = Nothing

-- `substitute e (x, e')` means e[e'/x]
substitute :: Expr PCF -> (Identifier, Expr PCF) -> Expr PCF
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

-- PCF terms
substitute (Ext Zero) s                    = Ext Zero
substitute (Ext Succ) s                    = Ext Succ

substitute (Ext (Fix e)) s                 = Ext $ Fix $ substitute e s

substitute (Ext (Case e1 e2 (y,e3))) (x,e) =
  let e1' = substitute e1 (x,e)
      e2' = substitute e2 (x,e)
  in if x == y then Ext $ Case e1' e2' (y,e3)
  else if y `Set.member` free_vars e then
    let y' = fresh_var y (free_vars e `Set.union` free_vars e3)
    in Ext $ Case e1' e2' (y', substitute (substitute e3 (y, Var y')) (x,e))
  else Ext $ Case e1' e2' (y, substitute e3 (x,e))

-- Keep doing small step reductions until normal form reached
multiStep :: Expr PCF -> (Expr PCF, Int)
multiStep e = multiStep' e 0

multiStep' :: Expr PCF -> Int -> (Expr PCF, Int)
multiStep' t n =
    case smallStep t of
      -- Normal form reached
      Nothing -> (t, n)
      -- Can do more reduction
      Just t' -> multiStep' t' (n+1)

