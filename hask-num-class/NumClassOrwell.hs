{-

   LambdaClub - Wednesday 23rd November 2016

   2+2=5: Exploring the power of Haskell's overloaded numerics
   Dominic Orchard

-}

{-# LANGUAGE FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables #-}

import Test.QuickCheck

-- *********************************************************
-- 2+2=5 version of Num

data Orwell = Zero | One | Two | Three | Four | Five
               deriving (Eq, Show)

fromOrwell :: Orwell -> Integer
fromOrwell Zero  = 0
fromOrwell One   = 1
fromOrwell Two   = 2
fromOrwell Three = 3
fromOrwell Four  = 4
fromOrwell Five  = 5


instance Num Orwell where
    Two + Two     = Five
    x + y         =
            fromInteger ((fromOrwell x) + (fromOrwell y))

    fromInteger 0 = Zero
    fromInteger 1 = One
    fromInteger 2 = Two
    fromInteger 3 = Three
    fromInteger 4 = Four
    fromInteger 5 = Five

    -- Saturating arithmetic version
    --fromInteger x = Five

    -- Modulo arithmetic version
    fromInteger x = fromInteger (x `mod` 5)
    negate        x = x

    -- Cheating for simplicity (and to hide error messages)
    x * y  = undefined
    abs    = undefined
    signum = undefined

-- We can use QuickCheck to see if we have the
-- usual associativity axioms hold

instance Arbitrary Orwell where
  arbitrary = oneof [return Zero, return One, return Two,
                     return Three, return Four, return Five]

assoc = \x y z -> (((x :: Orwell) + y) + z) == (x + (y + z))
unit x = x == x + Zero

-- e.g. quickCheck assoc
-- e.g. quickCheck unit
















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
