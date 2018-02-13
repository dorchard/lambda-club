module Main where

-- You'll need to install criterion:
-- cabal install criterion

import Data.Array.MArray
import Data.Array.IArray
import Data.Array.ST (STArray, runSTArray)
import Control.Monad.ST

import Criterion.Main

-- LAMBDA CLUBBBBBB!!!! Dominic

fib :: Int -> Integer
fib = fibUnfix fib

fibUnfix :: (Int -> Integer) -> Int -> Integer
fibUnfix _ 0 = 0
fibUnfix _ 1 = 1
fibUnfix f n = f (n - 1) + f (n - 2)

fibMemo :: Int -> Integer
fibMemo = (table !!)
  where
    table = map (fibUnfix fibMemo) [0..]

fibMemoArray :: Int -> Integer
fibMemoArray = tblLookup
  where
    maxSize = 30000
    tblLookup i = table ! i
    table = runSTArray $ do
        -- Create a new array of size maxSize
        arr <- newArray_ (0, maxSize)
        -- "for" loop to write the elements - !BUT! using itself
        mapM_ (\i -> writeArray arr i (fibUnfix tblLookup i)) [0..maxSize]
        return arr

-- Criterion code
-- ghc -O2 --make fib.hs
-- ./fib -o fib-results.html
main =
  defaultMain
    [  bgroup "fib" $
         map (\size -> bench ("fib/" ++ show size) $ whnf fibMemoArray size) sizes
      ++ map (\size -> bench ("fibMemo/" ++ show size) $ whnf fibMemo size) sizes
    ]
  where sizes = [5000,10000..30000]
