{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.Unfix
import Data.Function.ArrayMemoize

unfix [d|
  fib :: Int -> Integer
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n - 1) + fib (n - 2) |]

fibFast = arrayMemoFix (0, 10000) fib
