{- QuasiQuotes for convenience -}

{-# LANGUAGE TemplateHaskell #-}
module TH where

import GADTs (List(..), BoundedNat(..), Format(..), S, Z)
import Language.Haskell.TH hiding (Pred)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

nat, fmt :: QuasiQuoter

nat = defQuoter
  { quoteExp = quoteE . read
  , quoteType = quoteT
  } where
    quoteE :: Int -> Q Exp
    quoteE 0 = [| Z |]
    quoteE n = [| S $(quoteE (n - 1)) |]
    quoteT :: String -> Q Type
    quoteT s | (a, '<' : '=' : b) <- break (== '<') s
      = [t| BoundedNat $(quoteT a) $(quoteT b) |]
    quoteT s = qt (read s)
    qt :: Int -> Q Type
    qt 0 = [t| Z |]
    qt n = [t| S $(qt (n - 1)) |]

fmt = defQuoter
  { quoteExp = quote
  } where
    quote [] = [| FmtDone |]
    quote ('%' : 'd' : s) = [| FmtInt $(quote s) |]
    quote ('%' : 's' : s) = [| FmtString $(quote s) |]
    quote ('%' : 'a' : s) = [| FmtUser $(quote s) |]
    quote (c : s) =
      let (s1, s2) = break (== '%') s
      in [| FmtConst $(lift (c : s1)) $(quote s2) |]

defQuoter :: QuasiQuoter
defQuoter = QuasiQuoter undefined undefined undefined undefined
