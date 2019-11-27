module Lam.Semantics where

import Lam.Syntax

import qualified Data.Set as Set

-- Small-step operational semantics reduction
smallStep :: Expr PCF -> Maybe (Expr PCF)
smallStep (Var _) = Nothing
-- Beta reduction
smallStep (App (Abs x e) e') | isValue e' =
  Just (substitute e (x, e'))
-- App rules
smallStep (App e1 e2) =
  case smallStep e1 of
    -- App L rule -- (zeta 1, prioritised over zeta 2)
    Just e1' -> Just (App e1' e2)
    Nothing ->
      case smallStep e2 of
        Nothing -> Nothing
        -- App R rule -- (zeta 2)
        Just e2' -> Just (App e1 e2')
-- Abs rule -- (zeta 3)
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

smallStep (Ext (NatCase (Ext Zero) e1 _)) = Just e1

smallStep (Ext (NatCase (App (Ext Succ) n) _ (x,e2))) = Just $ substitute e2 (x,n)

smallStep (Ext (NatCase e e1 (x,e2))) =
  case smallStep e of
    Just e' -> Just (Ext (NatCase e' e1 (x,e2)))
    Nothing -> Nothing

smallStep (Ext (Pair e1 e2)) =
  case smallStep e1 of
    Just e1' -> Just $ Ext $ Pair e1' e2
    Nothing -> (
      case smallStep e2 of
        Just e2' -> Just $ Ext $ Pair e1 e2'
        Nothing -> Nothing
      )

smallStep (Ext (Fst (Ext (Pair e1 e2)))) = Just e1
smallStep (Ext (Snd (Ext (Pair e1 e2)))) = Just e2

smallStep (Ext (Fst e)) =
  case smallStep e of
    Just e' -> Just $ Ext $ Fst e'
    Nothing -> Nothing

smallStep (Ext (Snd e)) =
  case smallStep e of
    Just e' -> Just $ Ext $ Snd e'
    Nothing -> Nothing

smallStep (Ext (Case (Ext (Inl e)) (x,e1) _)) = Just $ substitute e1 (x,e)
smallStep (Ext (Case (Ext (Inr e)) _ (y,e2))) = Just $ substitute e2 (y,e)

smallStep (Ext (Case e (x,e1) (y,e2))) =
  case smallStep e of
    Just e' -> Just (Ext (Case e' (x,e1) (y,e2)))
    Nothing -> Nothing

smallStep (Ext (Inl e)) =
  case smallStep e of
    Just e' -> Just $ Ext $ Inl e'
    Nothing -> Nothing

smallStep (Ext (Inr e)) =
  case smallStep e of
    Just e' -> Just $ Ext $ Inr e'
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

substitute (Abs y e) s =
  let (y', e') = substitute_binding y e s in Abs y' e'

substitute (Sig e t) s = Sig (substitute e s) t

-- PCF terms
substitute (Ext Zero) s                    = Ext Zero
substitute (Ext Succ) s                    = Ext Succ

substitute (Ext (Fix e)) s                 = Ext $ Fix $ substitute e s

substitute (Ext (NatCase e e1 (y,e2))) s =
  let e'  = substitute e s
      e1' = substitute e1 s
      (y', e2') = substitute_binding y e2 s
  in Ext $ NatCase e' e1' (y', e2')

substitute (Ext (Pair e1 e2)) s =
  Ext $ Pair (substitute e1 s) (substitute e2 s)

substitute (Ext (Fst e)) s = Ext $ Fst $ substitute e s
substitute (Ext (Snd e)) s = Ext $ Snd $ substitute e s

substitute (Ext (Case e (x,e1) (y,e2))) s =
  let e' = substitute e s
      (x', e1') = substitute_binding x e1 s
      (y', e2') = substitute_binding y e2 s
  in Ext $ Case e' (x', e1') (y', e2')

substitute (Ext (Inl e)) s = Ext $ Inl $ substitute e s
substitute (Ext (Inr e)) s = Ext $ Inr $ substitute e s


-- substitute_binding x e (y,e') substitutes e' into e for y, but assumes e has just had binder x introduced
substitute_binding :: Identifier -> Expr PCF -> (Identifier, Expr PCF) -> (Identifier, Expr PCF)
substitute_binding x e (y,e')
  -- Name clash in binding - we are done
  | x == y = (x, e)
  -- If expression to be bound contains already bound variable
  | x `Set.member` free_vars e' =
    let x' = fresh_var x (free_vars e `Set.union` free_vars e')
    in (x', substitute (substitute e (x, Var x')) (y, e'))
  | otherwise = (x, substitute e (y,e'))

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

