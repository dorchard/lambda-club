{-

  Applications of Generalized Algebraic Data Types

-}

{-# LANGUAGE GADTs, TypeFamilies #-}
module GADTs where

import Prelude hiding (length, head, tail, map, filter, (!!), take)

-- ----- --
-- Lists --
-- ----- --

data S n
data Z

infixr 1 :.

data List n a where
  Nil :: List Z a
  (:.) :: a -> List n a -> List (S n) a

length :: List n a -> Int
length Nil = 0
length (_ :. as) = 1 + length as

head :: List (S n) a -> a
head (a :. _) = a

tail :: List (S n) a -> List n a
tail (_ :. as) = as

map :: (a -> b) -> List n a -> List n b
map _ Nil = Nil
map f (a :. as) = (f a) :. (map f as)

toList :: List n a -> [a]
toList Nil = []
toList (a :. as) = a : toList as

filter :: (a -> Bool) -> List n a -> [a]
filter _ Nil = []
filter p (a :. as) =
  case p a of
    True -> a : filter p as
    False -> filter p as

-- --------- ------- ------- --
-- Decorated natural numbers --
-- --------- ------- ------- --

data BoundedNat m n where
  Z :: BoundedNat Z n
  S :: BoundedNat m n -> BoundedNat (S m) (S n)

toInt :: BoundedNat m n -> Int
toInt Z = 0
toInt (S n) = 1 + toInt n

(!!) :: List (S n) a -> BoundedNat m n -> a
(a :. _) !! Z = a
(_ :. as) !! S n = as !! n

-- Can't get it wrong.
take :: BoundedNat m n -> List n a -> List m a
take Z _ = Nil
take (S n) (a :. as) = a :. take n as

type family Minus m n :: * where
  -- S ans Z are types
  Minus n Z = n
  Minus (S n) (S m) = Minus n m

drop' :: BoundedNat m n -> List n a -> List (Minus n m) a
drop' Z as = as
drop' (S n) (a :. as) = drop' n as

-- ---------- ----------- --
-- Evaluating expressions --
-- ---------- ----------- --

-- http://mads-hartmann.com/ocaml/2015/01/05/gadt-ocaml.html

data Expr a where
  Val :: a -> Expr a
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  (:==) :: Expr Int -> Expr Int -> Expr Bool
  (:<) :: Expr Int -> Expr Int -> Expr Bool

ex1 :: Expr Int
ex1 = Val 1337

ex2 :: Expr Bool
ex2 = Val (6 * 9 == (42 :: Int))

ex3 :: Expr Int
ex3 =
  If (Val 2 :< Val 4)
    (Val 42)
    (Val 0)

eval :: Expr a -> a
eval (Val a) = a
eval (If cond then_ else_) =
  case eval cond of
    True -> eval then_
    False -> eval else_
eval (a :== b) = eval a == eval b
eval (a :< b) = eval a < eval b

-- ------ ------- --
-- Format strings --
-- ------ ------- --

-- http://gallium.inria.fr/blog/format6/

data Format f r where
  FmtDone :: Format r r
  FmtConst :: String -> Format f r -> Format f r
  FmtInt :: Format f r -> Format (Int -> f) r
  FmtString :: Format f r -> Format (String -> f) r
  FmtUser :: Format f r -> Format ((a -> r) -> a -> f) r

sprintf :: Format f String -> f
sprintf = sprintf' ""

sprintf' :: String -> Format f String -> f
sprintf' acc FmtDone = acc
sprintf' acc (FmtConst s fmt) = sprintf' (acc ++ s) fmt
sprintf' acc (FmtInt fmt) = \n -> sprintf' (acc ++ show n) fmt
sprintf' acc (FmtString fmt) = \s -> sprintf' (acc ++ s) fmt
sprintf' acc (FmtUser fmt) = \f a -> sprintf' (acc ++ f a) fmt

printf :: Format f (IO ()) -> f
printf = printf' (return ())

printf' :: IO () -> Format f (IO ()) -> f
printf' acc FmtDone = acc
printf' acc (FmtConst s fmt) = printf' (acc >> putStr s) fmt
printf' acc (FmtInt fmt) = \n -> printf' (acc >> putStr (show n)) fmt
printf' acc (FmtString fmt) = \s -> printf' (acc >> putStr s) fmt
printf' acc (FmtUser fmt) = \f a -> printf' (acc >> f a) fmt

-- TH.hs defines quasiquotes to write Format values conveniently.
-- See Defs.hs for more examples.

----------------------------------------

---------- Show instances ----------

instance Show a => Show (List n a) where
  show = show . toList

instance Show (BoundedNat m n) where
  show = show . toInt

instance Show a => Show (Expr a) where
  show (Val a) = "(Val " ++ show a ++ ")"
  show (If a b c) = "(If " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"
  show (a :== b) = "(" ++ show a ++ " :== " ++ show b ++ ")"
  show (a :< b) = "(" ++ show a ++ " :< " ++ show b ++ ")"

instance Show (Format f r) where
  show FmtDone = "FmtDone"
  show (FmtConst s fmt) = "(FmtConst " ++ show s ++ " " ++ show fmt ++ ")"
  show (FmtInt fmt) = "(FmtInt " ++ show fmt ++ ")"
  show (FmtString fmt) = "(FmtString " ++ show fmt ++ ")"
  show (FmtUser fmt) = "(FmtUser " ++ show fmt ++ ")"

showFmt :: Format f r -> String
showFmt = show . showFmt'

showFmt' :: Format f r -> String
showFmt' FmtDone = ""
showFmt' (FmtConst s fmt) = s ++ showFmt' fmt
showFmt' (FmtInt fmt) = "%d" ++ showFmt' fmt
showFmt' (FmtString fmt) = "%s" ++ showFmt' fmt
showFmt' (FmtUser fmt) = "%a" ++ showFmt' fmt

