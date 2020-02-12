{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lam.HindleyMilner where

import Lam.Syntax
import Lam.PrettyPrint
import Data.Set (toList)

data TypeScheme = ForallTy [Identifier] Type

type Context = [(Identifier, TypeScheme)]

-- Top-level of Algorithm W (Hindley Milner algorithm)
inferType :: Context -> Expr PCF -> Maybe Type
inferType ctxt ast = let (s, ty, _) = infer ctxt ast 0 in Just (substitute s ty)

type TypeVariable = String
type Substitution = TypeVariable -> Type

-- error helper
unificationUnknown :: String -> TypeVariable -> a
unificationUnknown msg var =
  error $ "Unification variable in " ++ msg ++ " unknown for " ++ var

-- Inner main function of Algorithm W
-- Takes:
--   * a typing context
--   * the expression whose type we are infering
--   * an integer acting as the seed for doing gensym
-- Returns (if succesful) a:
--   * a substitution on type variables
--   * a type (if succesful)
--   * an updated gensym seed
infer :: Context -> Expr PCF -> Int -> (Substitution, Type, Int)
infer ctxt (Abs var _ e) fv =
  let alpha     = "a" ++ show fv
      (s , tyB , fv') = infer ((var , ForallTy [] (TyVar alpha)) : ctxt) e (fv+1)
  in (s, FunTy (s alpha) tyB, fv')

infer ctxt (App e1 e2) fv =
  let (s1 , t1, fv') = infer ctxt e1 fv
      (s2, t2, fv'') = infer ctxt e2 fv'
      alpha          = "a" ++ show fv''
  in do
    case mgu (substitute s2 t1) (FunTy t2 (TyVar alpha)) of
      Just s3 -> (s3 <.> s2 <.> s1, s3 alpha, fv''+1)
      _ -> error "Cannot unify in app"

infer ctxt (Var x) fv =
    case lookup x ctxt of
      Just (ForallTy alphas ty) -> 
        let (tyFreshened, fv') = freshen alphas ty fv
        in (empty, tyFreshened, fv')
      Nothing -> error $ "I don't know the type of " ++ x
  where
    freshen [] ty fv = (ty, fv)
    freshen (x:xs) ty fv = 
      freshen xs (substitute (singleton x (TyVar $ "a" ++ show fv)) ty) (fv + 1)

infer ctxt (GenLet x e1 e2) fv =
  let (s1, t1, fv') = infer ctxt e1 fv
      alphas = freeVars t1 -- setminus ftv(substitute s1 ctxt)
      (s2, t2, fv'') = infer ((x, ForallTy (toList alphas) t1) : substitute s1 ctxt) e2 fv'
  in (s2 <.> s1, t2, fv'')

infer ctxt (Ext Zero) fv = (empty, NatTy, fv)
infer ctxt (Ext Succ) fv = (empty, FunTy NatTy NatTy, fv)

infer ctxt t fv = 
  error $ "I don't know how to infer the type of " ++ pprint t


-- (a -> Bool) `mgu` (Int -> b)
--   = Just [a |-> Int, b |-> Bool]

-- t1 `mgu` t2 = Just s   =>  s(t1) = s(t2)
-- forall s' . s'(t1)=s'(t2) => exists s'' . (s'' . s) = s'

-- {a -> Int} <.> {b -> (a, Int)}
--    = {b -> (Int, Int), a -> Int}

empty :: Substitution
empty var = TyVar var

singleton :: TypeVariable -> Type -> Substitution
singleton var ty =
  \var' -> if var == var' then ty else TyVar var'

-- empty <.> s = s
-- s <.> empty = s

-- substitute s2 (substitute s1 t) =
--  substitute (s2 <.> s1) t

(<.>) :: Substitution -> Substitution -> Substitution
s2 <.> s1 =
  \v ->
    if s1 v == TyVar v
      then s2 v
      else substitute s2 (s1 v)

-- Calculate the most general unified of two types
mgu :: Type -> Type -> Maybe Substitution
mgu (FunTy t1 t2) (FunTy t1' t2') = do
  s <- mgu t1 t1'
  s' <- mgu t2 t2'
  Just $ s <.> s'
mgu (ProdTy t1 t2) (ProdTy t1' t2') = do
  s <- mgu t1 t1'
  s' <- mgu t2 t2'
  Just $ s <.> s'
mgu (SumTy t1 t2) (SumTy t1' t2') = do
  case mgu t1 t1' of
    Just s -> case mgu t2 t2' of
               Just s' -> Just $ s <.> s'
               Nothing -> Nothing
    Nothing -> Nothing
mgu NatTy NatTy = Just empty
mgu (TyVar a) ty = Just $ singleton a ty
mgu ty (TyVar a) = Just $ singleton a ty
mgu t1 t2 = error $ "Cannot unify " ++ pprint t1 ++ " and " ++ pprint t2


-- Apply a substitution to a type
class Substitutable t where
  substitute :: Substitution -> t -> t

instance Substitutable Type where
  substitute subst NatTy = NatTy
  substitute subst (FunTy t1 t2)  = FunTy (substitute subst t1) (substitute subst t2)
  substitute subst (ProdTy t1 t2) = ProdTy (substitute subst t1) (substitute subst t2)
  substitute subst (SumTy t1 t2)  = SumTy (substitute subst t1) (substitute subst t2)
  substitute subst (TyVar var)    = subst var
  substitute subst (Forall{}) =
    error "Polymorphic lambda calculus type showing up in ML types"

instance Substitutable Context where
  substitute subst ctxt =
    map (\(var, ForallTy ids ty) -> (var, ForallTy ids (substitute subst ty))) ctxt



