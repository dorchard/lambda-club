{-

   LambdaClub - Wednesday 23rd November 2016

   2+2=5: Exploring the power of Haskell's overloaded numerics
   Dominic Orchard

-}

{-# LANGUAGE FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables #-}

import Control.Monad.State

-- Beginning: Haskell's numerals are overloaded
-- e.g.
-- In GHCi, try
-- :t 1
-- :t (+)
-- :t (*)
-- :t (-)

two :: Num t => t
two = 2

plus :: Num t => t -> t -> t
plus = (+)

gradient m c x = m * x + c

-- This overloading is provided by the Num class in Haskell
-- :info Num

-- Default instances from
--   Word, Integer, Int, Float, Double

-- We can create our own instances!


-- ***************************************************
-- Booleans as Num (a bit like in C)

instance Num Bool where
   x + y = x || y
   x * y = x && y
   negate x = not x
   fromInteger 0 = False
   fromInteger _ = True
   abs x         = x
   signum x      = 1


boolExample = ((1 + 3) * 6) :: Bool

-- This shows the role of 'fromInteger'

-- ***************************************************
-- Overloading a num on lists

instance Num a => Num [a] where
   xs + ys       = zipWith (+) xs ys
   xs * ys       = zipWith (*) xs ys
   negate xs     = map negate xs
   fromInteger x = [fromInteger x]
   abs x         = map abs x
   signum x      = map signum x

-- e.g. gradient ([1, 2, 3] :: [Int]) [4, 5, 6] [12, 10, 1]

-- *********************************************************
-- Peano numbers

data Nat = Z | S Nat deriving (Eq, Show)

instance Num Nat where
    fromInteger 0 = Z
    fromInteger n | n > 0 = S (fromInteger (n - 1))
                  | otherwise = Z

    n + Z = n
    n + (S m) = S (n + m)

    n * Z = Z
    n * (S m) = n + (n * m)

    n - Z = n
    Z - n = Z
    (S n) - (S m) = n - m

    abs x = x
    signum Z = Z
    signum _ = (S Z)

natToInteger Z = 0
natToInteger (S n) = 1 + (natToInteger n)

-- **************************************************
-- Syntax trees which code generate C

data Expr a = Plus (Expr a) (Expr a)
          | Mult (Expr a) (Expr a)
          | Neg (Expr a)
          | Abs (Expr a)
          | Const a
          | Signum (Expr a)

instance Num a => Num (Expr a) where
    x + y          = Plus x y
    x * y          = Mult x y
    negate x       = Neg x
    abs x          = Abs x
    signum x       = Signum x
    fromInteger x  = Const (fromInteger x)

instance Show a => Show (Expr a) where
  show e =
    concatMap (\i -> "int v" ++ (show i) ++ ";\n") [0..maxVar]
      ++ code
      ++ "return " ++ val ++ ";"
    where
      (val, (maxVar, code)) = runState (codeGen e) (0, "")

cGradient
  :: Num a
  => (Expr a) -> (Expr a) -> (Expr a) -> (Expr a)
cGradient = gradient

factorial :: Num t => Integer -> t
factorial 0 = 1
factorial n = (fromInteger n) * (factorial (n-1))

-- e.g. (factorial 7) :: Expr

codeGen :: Show a => (Expr a) -> State (Int, String) String
codeGen (Plus x y) = do
    x' <- codeGen x
    y' <- codeGen y
    c <- freshName
    seqCode (assgn c (x' ++ " + " ++ y'))
    return $ c
codeGen (Mult x y) = do
    x' <- codeGen x
    y' <- codeGen y
    c <- freshName
    seqCode (assgn c (x' ++ " * " ++ y'))
    return $ c
codeGen (Neg x) = do
    c <- codeGen x
    return ("-(" ++ c ++ ")")
codeGen (Abs x) = do
    c <- codeGen x
    return ("abs(" ++ c ++ ")")
codeGen (Const c) =
    return $ show c
codeGen (Signum x) = do
    c <- codeGen x
    a <- freshName
    seqCode (assgn a (c ++ " / abs(" ++ c ++ ")"))
    return a

freshName :: State (Int, String) String
freshName = do
  (n, c) <- get
  put (n+1, c)
  return $ "v" ++ show n

seqCode :: String -> State (Int, String) ()
seqCode c' = do
  (n, c) <- get
  put (n, c ++ c' ++ ";\n")

assgn :: String -> String -> String
assgn a e = a ++ " = " ++ e

-- *********************************************************
-- Interval analysis for free!

data Interval a = I a a deriving (Show, Eq)

instance (Ord a, Num a) => Num (Interval a) where
    (I l u) + (I l' u') = I (l + l') (u + u')
    (I l u) * (I l' u') =
      I (minimum [l * l', l * u', l' * u, l' * u'])
        (maximum [l * l', l * u', l' * u, l' * u'])
    (I l u) - (I l' u') = I (l - u') (u - l')

    fromInteger x  = I (fromInteger x) (fromInteger x)
    abs    (I l u) | l < 0 && u >= 0 = I 0 u
                   | l < 0 && u < 0  = I (abs u) (abs l)
                   | otherwise       = I l u
    signum (I l u) = I (signum l) (signum u)

intervalExample :: Interval Float
intervalExample = gradient (I 0.5 1) (I 1 10) (I 1.5 2.5)


-- *********************************************************
-- Holy-generalisation Batman!
-- All Applicatives on Num are Num

instance {-# OVERLAPPABLE #-}
    (Applicative f, Num a) => Num (f a) where
   xs + ys   = pure (+) <*> xs <*> ys
   xs * ys   = pure (+) <*> xs <*> ys
   negate xs = fmap negate xs
   fromInteger x = pure (fromInteger x)
   abs x     = fmap abs x
   signum x  = fmap signum x
