module Lam.Types where

import Lam.Syntax
import Lam.PrettyPrint

{-

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

*********************************
G |- e <= A    check
**********************************

G, x : A |- e <= B
--------------------------- abs
G |- (\x -> e) <= A -> B

G |- e => A'   A' == A
--------------------------- synthCheck
G |- e <= A

**********************************
G |- e => A    synth
**********************************

(x : A) in G
--------------- var
G |- x => A

G |- e1 => A -> B    G |- e2 <= A
----------------------------------- app
G |- e1 e2 => B

G |- e <= A
------------------- checkSynth
G |- (e : A) => A

[IF CHURCH SYNTAX, but NOT IN LCORE/CURRY SYNTAX]
G, x : A |- e => B
--------------------------- abs
G |- (\(x : A) -> e) => B

-}

type Context = [(Identifier, Type)]

check :: Context -> Expr PCF -> Type -> Bool

-- abs
check gamma (Abs x expr) (FunTy tyA tyB) =
  check ([(x, tyA)] ++ gamma) expr tyB

-- synthCheck
check gamma expr tyA =
  case synth gamma expr of
    Nothing -> False
    Just tyA' -> tyA == tyA'

synth :: Context -> Expr PCF -> Maybe Type

-- var
synth gamma (Var x) =
  lookup x gamma

-- zero
synth gamma (Ext Zero) =
  Just NatTy

-- app (special for form of top-level definitions)
synth gamma (App (Abs x (Sig e1 tyB)) (Sig e2 tyA)) =
   if check ([(x, tyA)] ++ gamma) e1 tyB
    then Just tyB
    else Nothing

-- app
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


-- checkSynth

synth gamma (Sig e ty) =
  if check gamma e ty
    then Just ty
    else error $ "Trying to check (" ++ pprint e ++ ") against " ++ pprint ty

-- catch all (cannot synth here)
synth gamma e =
   error $ "Cannot synth for " ++ pprint e