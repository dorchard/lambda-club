{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lam.HindleyMilner where

import Lam.Syntax

data TypeScheme = ForallTy [Identifier] Type

type Context = [(Identifier, TypeScheme)]
 
-- Top-level of Algorithm W (Hindley Milner algorithm)
inferType :: Context -> Expr PCF -> Maybe Type
inferType ctxt ast = let (_, ty, _) = infer ctxt 0 ast in Just ty

type Substitution = Identifier -> Type

-- Inner main function of Algorithm W
-- Takes:
--   * a typing context
--   * an integer acting as the seed for doing gensym
--   * the expression whose type we are infering
-- Returns (if succesful) a:
--   * a substitution on type variables
--   * a type (if succesful)
--   * an updated gensym seed
infer :: Context -> Int -> Expr PCF -> (Substitution, Type, Int)
infer = error "TODO"

-- Calculate the most general unified of two types
mgu :: Type -> Type -> Maybe Substitution
mgu = error "TODO"

-- Apply a substitution to a type
class Substitutable t where
  substitute :: Substitution -> t -> t

instance Substitutable Type where
  substitute subst NatTy = NatTy
  substitute subst (FunTy t1 t2)  = FunTy (substitute subst t1) (substitute subst t2)
  substitute subst (ProdTy t1 t2) = ProdTy (substitute subst t1) (substitute subst t2)
  substitute subst (SumTy t1 t2)  = SumTy (substitute subst t1) (substitute subst t2)
  substitute subst _ = error "TODO the rest"

instance Substitutable Context where
  substitute subst ctxt =
    map (\(var, ForallTy ids ty) -> (var, ForallTy ids (substitute subst ty))) ctxt



