module Lam.Semantics where

import Lam.Syntax
import Lam.Options

import qualified Data.Set as Set

type Reducer a = a -> Maybe a

fullBeta :: Reducer (Expr PCF)
fullBeta (Var _) = Nothing
fullBeta (App (Abs x e) e') = beta e x e'
fullBeta (App e1 e2) =
  -- Prefer fully zeta1 reducing before zeta2 reducing
  case zeta1 fullBeta e1 e2 of
    Just e -> Just e
    Nothing -> zeta2 fullBeta e1 e2
fullBeta (Abs x e) = zeta3 fullBeta x e
fullBeta (Sig e _) = Just e
fullBeta (Ext e) = reducePCF fullBeta (Ext e)

callByName :: Reducer (Expr PCF)
callByName (Var _) = Nothing
callByName (App (Abs x e) e') = beta e x e'
callByName (App e1 e2) = zeta1 callByName e1 e2
callByName (Abs x e) = Nothing
callByName (Sig e _) = Just e
callByName (Ext e) = reducePCF callByName (Ext e)

callByValue :: Reducer (Expr PCF)
callByValue (Var _) = Nothing
callByValue (App (Abs x e) e') | isValue e' = beta e x e'
callByValue (App e1 e2) | isValue e1 = zeta2 callByValue e1 e2
callByValue (App e1 e2) = zeta1 callByValue e1 e2
callByValue (Abs x e) = Nothing
callByValue (Sig e _) = Just e
callByValue (Ext e) = reducePCF callByValue (Ext e)

-- Base case
beta :: Expr PCF -> Identifier -> Expr PCF -> Maybe (Expr PCF)
beta e x e' = Just (substitute e (x, e'))

-- Inductive rules
zeta1 :: Reducer (Expr PCF) -> Expr PCF -> Expr PCF -> Maybe (Expr PCF)
zeta1 step e1 e2 = (\e1' -> App e1' e2) <$> step e1

zeta2 :: Reducer (Expr PCF)  -> Expr PCF -> Expr PCF -> Maybe (Expr PCF)
zeta2 step e1 e2 = (\e2' -> App e1 e2') <$> step e2

zeta3 :: Reducer (Expr PCF)  -> Identifier -> Expr PCF -> Maybe (Expr PCF)
zeta3 step x e = (\e' -> Abs x e') <$> step e

-- Reducer for the extended PCF syntax
reducePCF :: Reducer (Expr PCF) -> Reducer (Expr PCF)
reducePCF step (Ext (Fix e)) =
  case step e of
    Just e' -> Just (Ext $ Fix e')
    Nothing -> Just $ App e (Ext $ Fix e)

reducePCF step (Ext (NatCase (Ext Zero) e1 _)) = Just e1

reducePCF step (Ext (NatCase (App (Ext Succ) n) _ (x,e2))) = Just $ substitute e2 (x,n)

reducePCF step (Ext (NatCase e e1 (x,e2))) =
  (\e' -> Ext (NatCase e' e1 (x,e2))) <$> step e

reducePCF step (Ext (Pair e1 e2)) =
  case step e1 of
    Just e1' -> Just $ Ext $ Pair e1' e2
    Nothing -> (\e2' -> Ext $ Pair e1 e2') <$> step e2

reducePCF step (Ext (Fst (Ext (Pair e1 e2)))) = Just e1
reducePCF step (Ext (Snd (Ext (Pair e1 e2)))) = Just e2

reducePCF step (Ext (Fst e)) = (\e' -> Ext $ Fst e') <$> step e
reducePCF step (Ext (Snd e)) = (\e' -> Ext $ Snd e') <$> step e

reducePCF step (Ext (Case (Ext (Inl e)) (x,e1) _)) = Just $ substitute e1 (x,e)
reducePCF step (Ext (Case (Ext (Inr e)) _ (y,e2))) = Just $ substitute e2 (y,e)

reducePCF step (Ext (Case e (x,e1) (y,e2))) =
  (\e' -> Ext (Case e' (x,e1) (y,e2))) <$> step e

reducePCF step (Ext (Inl e)) = (\e' -> Ext $ Inl e') <$> step e
reducePCF step (Ext (Inr e)) = (\e' -> Ext $ Inr e') <$> step e

-- other Ext terms
reducePCF step (Ext _) = Nothing

-- Non Ext terms
reducePCF _ _ = error "invalid term"

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
multiStep :: [Option] -> Expr PCF -> (Expr PCF, Int)
multiStep opts e | isCBV opts = multiStep' callByValue e 0
multiStep opts e | isCBN opts = multiStep' callByName e 0
multiStep _    e              = multiStep' fullBeta e 0

multiStep' :: Reducer (Expr PCF) -> Expr PCF -> Int -> (Expr PCF, Int)
multiStep' step t n =
  case step t of
    -- Normal form reached
    Nothing -> (t, n)
    -- Can do more reduction
    Just t' -> multiStep' step t' (n+1)
