{-

   LambdaClub - Wednesday 23rd November 2016

   2+2=5: Exploring the power of Haskell's overloaded numerics
   Dominic Orchard

To run this example you will need to do:
  cabal install synthesizer-core

And also install the 'sox' tool.

-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import qualified Synthesizer.Plain.Play as Play
import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Plain.Control as Ctrl
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Filter.NonRecursive as Filt
import qualified Synthesizer.Basic.Wave as Wave
import qualified Synthesizer.Generic.Signal as Signal
import qualified Synthesizer.Plain.Cut as P
import qualified Synthesizer.Generic.Cut as G

play :: Sig.T Double -> IO ()
play s = (Play.monoToInt16 (44100 ::Double) s) >> return ()


-- Tones as Num

instance Num (Sig.T Double) where
   fromInteger n =
       P.takeUntilPause 0.05 1000 envWave
     where
     envWave = Filt.envelope (Ctrl.exponential 10000 1) wave
     wave = Osci.static Wave.sine 1 tone
     tone = (0.01 * ((fromInteger n) / 24) :: Double)

   x + y = x `mappend` y
   x * y = x `Signal.mix` y

   negate x = reverse x
   abs x    = reverse (G.drop 10000 (reverse env))
     where env = Filt.envelope (Ctrl.exponential 5000 1) x
   signum x = fromInteger 1

gradient m c x = m * x + c

factorial :: Num t => Integer -> t
factorial 0 = 1
factorial n = (fromInteger n) * (factorial (n-1))

fib x y z f 0 = f x
fib x y z f 1 = f y
fib x y z f 2 = f z
fib x y z f n = fib x y z f (n - 1) + fib x y z f (n - 2)

example = play $ (fib 20 25 30 id 5) * (fib 50 45 40 abs 5)