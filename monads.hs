-- Hello
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


import Prelude hiding (Monad(..))
import qualified Prelude as P

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv n 0 = Nothing
safeDiv n m = Just (n `div` m)

--foo n m p =
--   (safeDiv n m) + p

fooSafe n m p =
  case safeDiv n m of
    Just m' -> Just $ m' + p
    Nothing -> Nothing

data Expr = Add Expr Expr | Sub Expr Expr | Div Expr Expr | Const Int

bind :: (a -> Maybe b) -> Maybe a -> Maybe b
bind k ma =
  case ma of
    Nothing -> Nothing
    Just a  -> k a

instance Monad Maybe where
  return = Just
  (>>=) = flip bind

returnMaybe :: a -> Maybe a
returnMaybe = Just

interp :: Expr -> Maybe Int
interp (Add e1 e2) = do
  {-
  case interp e1 of
    Nothing -> Nothing
    Just n ->
      case interp e2  of
        Nothing -> Nothing
        Just m -> Just $ n + m -}

  -- interp e1 >>= (\n -> interp e2 >>= (\m -> Just (n + m)))
  n <- interp e1
  m <- interp e2
  return (n + m)


interp (Sub e1 e2) = do
  n <- interp e1
  m <- interp e2
  return (n - m)

interp (Div e1 e2) = do
  n <- interp e1
  m <- interp e2
  safeDiv n m

interp (Const n) = return n

-----------------------------------------------------------------

(>>>>=) :: IO a -> (a -> IO b) -> IO b
(>>>>=) = (P.>>=)

echo = getLine >>>>= putStrLn


comp :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
comp f g = \x -> f x >>= g

-- return `comp` f = f
-- f `comp` return = f
-- (f `comp` g) `comp` h = f `comp` (g `comp` h)

-- class Functor (f :: * -> *) where
--    fmap :: (a -> b) -> f a -> f b

--    fmap id = id
--    fmap (g . f) = fmap g . fmap f

class Functor m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
  -- (return x) >>= f  ===  f x
  -- ma >>= return   === ma
  -- (ma >>= f) >>= g === ma >>= (\x -> f x >>= g)

class MonoidMonad (m :: * -> *) where
  mmap    :: (a -> b) -> m a -> m b
  join    :: m (m a) -> m a
  unit    :: a -> m a

-- join (unit x)      = x   (x :: m a)
-- join (mmap unit y) = y   (y :: m a)
-- join (join x) = join . (mmap join x)
--                          (z :: m (m (m a))

instance MonoidMonad Maybe where
  unit = Just
  mmap f Nothing = Nothing
  mmap f (Just x) = Just (f x)
  join Nothing         = Nothing
  join (Just Nothing)  = Nothing
  join (Just (Just x)) = Just x

instance Monad m => MonoidMonad m where
  unit = return
  join = (\ma -> ma >>= id) -- ma :: m (m a)
                            -- (\ma -> ma >>= id) :: m a
  mmap f = \ma -> ma >>= (return . f)

instance {-# OVERLAPS #-} MonoidMonad m => Monad m where
  return = unit
  -- (>>=) :: m a -> (a -> m b) -> m b
  ma >>= f = join (mmap f ma)

{-

  -- ....

  -- forall x : a,  f : a -> m b
  --
  --   (return x) >>= f  ===  f x

  --      [do y <- return x; f y   ===   f x]

  -- for all  ma :: m a
  --  ma >>= return   === ma

  --     [do x <- ma; return x    ===  m a]

  -- for all ma :: m a , f : a -> m b, g : b -> m c
  --   (ma >>= f) >>= g === ma >>= (\x -> f x >>= g)

  --     [do y <- (do x <- ma; f x); g y  ===  do x <- ma; y <- f x; g y]


  Monad m
   Gam |- e1 : m a    Gam, x : a |- e2 : m b
----------------------------------------------
   Gam |- do {x <- e1; e2} : m b
~>
  e1 >>= (\x -> e2)

===
do
  x <- e1
  e2

-}

-- impureExpression ~>  (s, a) -> (s', b)

-- State :: * -> * -> *
-- State s :: * -> *
data State s a = State { unWrap :: s -> (a, s) }

put :: s -> State s ()
put ns = State (\_ -> ((), ns))

get :: State s s
get = State (\s -> (s, s))

instance Monad (State s) where
  return :: a -> State s a
  return x = State (\s -> (x, s))

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State k) >>= f =
    State (\s0 -> 
       let (a, s1) = k s0 
           (State k') = f a
           (b, s2) = k' s1
       in (b, s2)
      )

increment :: State Int ()
increment = do
  x <- get
  put (x + 1)

(>>) :: Monad m => m a -> m b -> m b
f >> g = f >>= (\_ -> g)
