{-

  Crafting types so programs can't go wrong
  =========================================

  Algebraic data types

-}

module ADTs where

import Prelude (Char, String, Int, Bool(..), Num(..), Show(..), Eq(..), Ord(..), (.), error, undefined)

-- -------- --
-- Booleans --
-- -------- --

{-
data Bool = False | True
-}

ifthenelse :: Bool -> a -> a -> a
ifthenelse b t f =
  case b of
    True -> t
    False -> f

(&&) :: Bool -> Bool -> Bool
False && _ = False
True && b = b

(||) :: Bool -> Bool -> Bool
True || _ = True
False || b = b

-- -------- ------ --
-- Optional values --
-- -------- ------ --

data Maybe a = Nothing | Just a

(<.>) :: Maybe a -> Maybe b -> Maybe (a, b)
Nothing <.> _ = Nothing
Just _ <.> Nothing = Nothing
Just a <.> Just b = Just (a, b)

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just a <|> _ = Just a
Nothing <|> mb = mb

-- ------ --
-- Tuples --
-- ------ --

data Pair a b = Pair a b
  deriving Show
{-

data (,) a b = (,) a b
data (a, b) = (a, b)

data (a, b, c) = (a, b, c)

-}

data Unit = Unit
{-

data () = ()

-}

exPair :: Pair (Maybe ()) Int
exPair = Pair (Just ()) 0

exPair' :: (Maybe (), Int)
exPair' = (Just (), 0)

exTriple :: (Char, Char, String)
exTriple = ('X', 'K', "CD")

-- ----- --
-- Lists --
-- ----- --

data List a = Nil | Cons a (List a)
{-
data [a] = [] | a : [a]
-}

length :: [a] -> Int
length [] = 0
length (_ : as) = 1 + length as

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (a : _) = Just a

maybeTail :: [a] -> Maybe ([a])
maybeTail [] = Nothing
maybeTail (_ : as) = Just as

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x : xs) = x : (take (n - 1) xs)
take _ [] = []

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n (_ : xs) = drop (n - 1) xs
drop _ [] = []

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a : as) = (f a) : (map f as)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (a : as) =
  case p a of
    True -> a : (filter p as)
    False -> filter p as

head :: [a] -> a
head (a : _) = a
head [] = error "Empty list"

-- ----- ------------ --
-- Smart constructors --
-- ----- ------------ --

data S n
data Z

type One = S Z
type Two = S (S Z)
type Three = S Two

data List_ n a = List_ [a]

nil :: List_ Z a
nil = List_ []

cons :: a -> List_ n a -> List_ (S n) a
cons a (List_ as) = List_ (a : as)

-- But pattern matching is still a problem...


-- ---------- ----------- --
-- Evaluating expressions --
-- ---------- ----------- --

data Value = VBool Bool | VInt Int
  deriving Show

data Expr
  = Val Value
  | If Expr Expr Expr
  | Expr :== Expr
  | Expr :< Expr
  deriving Show

int :: Int -> Expr
int = Val . VInt

bool :: Bool -> Expr
bool = Val . VBool

ex1, ex2, ex3 :: Expr

ex1 = int 1337

ex2 = bool (6 * 9 == (42 :: Int))

ex3 =
  If (int 2 :< int 4)
    (int 42)
    (int 0)

eval :: Expr -> Maybe Value
eval (Val v) = Just v
eval (If cond then_ else_) =
  case eval cond of
    Just (VBool True) -> eval then_
    Just (VBool False) -> eval else_
    _ -> Nothing
eval (e :== f) =
  case (eval e, eval f) of
    (Just (VInt n), Just (VInt m)) -> Just (VBool (n == m))
    _ -> Nothing
eval (e :< f) =
  case (eval e, eval f) of
    (Just (VInt n), Just (VInt m)) -> Just (VBool (n < m))
    _ -> Nothing

evalInt :: Expr -> Maybe Int
evalInt e = case eval e of
  Just (VInt n) -> Just n
  _ -> Nothing

evalBool :: Expr -> Maybe Bool
evalBool e = case eval e of
  Just (VBool b) -> Just b
  _ -> Nothing
