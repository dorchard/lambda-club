module Lam.Types where

import Lam.Syntax
import Lam.PrettyPrint

{-

*************************************************************
Declarative specification of the simply-typed lambda calculus
*************************************************************
Recall contexts are like lists of variable-type assumptions


G ::=  G, x : A | .

       (x : A) in G
var ----------------------
       G |- x : A

     G |- e1 : A -> B      G |- e2 : A
app ---------------------------------------
    G |- e1 e2 : B

      G, x : A |- e : B
abs ------------------------
      G |- \x -> e : A -> B

-}

-- Represent contexts as lists
type Context = [(Identifier, Type)]

{-

Bidirectional checking
*********************************
G |- e <= A    check
**********************************
-}

check :: Context -> Expr PCF -> Type -> Bool

{--

G, x : A |- e <= B
--------------------------- abs
G |- (\x -> e) <= A -> B

-}
check gamma (Abs x expr) (FunTy tyA tyB) =
  check ([(x, tyA)] ++ gamma) expr tyB

{--

G |- e => A'   A' == A
--------------------------- synthCheck
G |- e <= A

--}

check gamma expr tyA =
  case synth gamma expr of
    Nothing -> False
    Just tyA' -> tyA == tyA'

{-
Bidirectional synthesis
**********************************
 G |- e => A    synth
**********************************
-}

synth :: Context -> Expr PCF -> Maybe Type

{-

(x : A) in G
--------------- var
G |- x => A

-}

synth gamma (Var x) =
  lookup x gamma

-- zero (we will add more rules for PCF soon)
synth gamma (Ext Zero) =
  Just NatTy

{-

The following is a special form of (app) which
is useful for doing top-level definitions in our style,
which are of the form (\x -> e) (e' : A).

This is equivalent to combining the synthesis for general
application (below, (app) rule) with the synthesis rule we can have
if we have Church-style syntax

      G, x : A |- e => B
      -------------------------------------- abs-Church
      G |- (\(x : A) -> e) => A -> B

i.e., we know we have a signature for the argument.

-}

-- app (special for form of top-level definitions)
synth gamma (App (Abs x e1) (Sig e2 tyA)) =
   synth ([(x, tyA)] ++ gamma) e1

{-

  G |- e1 => A -> B    G |- e2 <= A
  ----------------------------------- app
  G |- e1 e2 => B

-}

synth gamma (App e1 e2) =
  -- Synth the left-hand side
  case synth gamma e1 of
    Just (FunTy tyA tyB) ->
      -- Check the right-hand side
      if check gamma e2 tyA
        -- Yay!
        then Just tyB
        else error $ "Expecting (" ++ pprint e2 ++ ") to have type " ++ pprint tyA

    Just t ->
      error $ "Expecting (" ++ pprint e1 ++ ") to have function type but got " ++ pprint t

    Nothing ->
      error $ "Expecting (" ++ pprint e1 ++ ") to have function type."


{-

  G |- e <= A
  ------------------- checkSynth
  G |- (e : A) => A

-}

-- checkSynth
synth gamma (Sig e ty) =
  if check gamma e ty
    then Just ty
    else error $ "Trying to check (" ++ pprint e ++ ") against " ++ pprint ty

-- catch all (cannot synth here)
synth gamma e =
   error $ "Cannot synth for " ++ pprint e
