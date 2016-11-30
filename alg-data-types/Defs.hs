{- More examples -}

{-# LANGUAGE QuasiQuotes #-}
module Defs where

import Prelude hiding (length, head, tail, map, filter, (!!), take)
import GADTs
import TH

-- nat: Natural numbers, as types and as values of type BoundedNat.

-- fourLessThanFive :: BoundedNat (S (S (S (S Z)))) (S (S (S (S (S Z)))))
-- fourLessThanFive = S (S (S (S Z)))

fourLessThanFive :: [nat|4 <= 5|]
fourLessThanFive = [nat|4|]

oneTwoThree :: List [nat|3|] Int
oneTwoThree = 1 :. 2 :. 3 :. Nil

-- fmt: Format strings

fmt1 :: Format (Int -> String -> r) r
fmt1 = [fmt|this is a number: %d; this is a string: %s|]

fmt2 :: Format ((Bool -> r) -> Bool -> r) r
fmt2 = [fmt|(42 == 6 * 9) == %a|]

sfmt1 :: Int -> String -> String
sfmt1 = sprintf fmt1

sfmt2 :: (Bool -> String) -> Bool -> String
sfmt2 = sprintf fmt2
