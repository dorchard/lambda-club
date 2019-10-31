{
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Lam.Lexer (Token(..),scanTokens,symString
                 ,getPos, constrString) where

import Data.Text (Text)
import Lam.FirstParameter
import GHC.Generics (Generic)

}

%wrapper "posn"

$digit  = 0-9
$alpha  = [a-zA-Z\_\-\=]
$lower  = [a-z]
$upper  = [A-Z]
$eol    = [\n]
$alphanum  = [$alpha $digit \_]
@sym    = $lower ($alphanum | \')*
@constr = ($upper ($alphanum | \')* | \(\))
@int    = \-? $digit+
@charLiteral = \' ([\\.]|[^\']| . ) \'
@stringLiteral = \"(\\.|[^\"]|\n)*\"

@langPrag = [a-z]+

tokens :-

  $white*$eol                   { \p s -> TokenNL p }
  $eol+                         { \p s -> TokenNL p }
  $white+                       ;
  "--".*                        ;
  @constr                       { \p s -> TokenConstr p s }
  lang.@langPrag                { \p s -> TokenLang p s }
  let                           { \p s -> TokenLet p }
  in                            { \p s -> TokenIn p }
  succ                          { \p s -> TokenSucc p }
  @sym				                  { \p s -> TokenSym p s }
  "->"                          { \p s -> TokenArrow p }
  \\                            { \p s -> TokenLambda p }
  \=                            { \p s -> TokenEq p }
  \(                            { \p s -> TokenLParen p }
  \)                            { \p s -> TokenRParen p }
  \:                            { \p s -> TokenSig p }
  "?"                           { \p _ -> TokenHole p }

{

data Token
  = TokenLang   AlexPosn String
  | TokenLet    AlexPosn
  | TokenIn     AlexPosn
  | TokenLambda AlexPosn
  | TokenSym    AlexPosn String
  | TokenSucc   AlexPosn
  | TokenArrow  AlexPosn
  | TokenEq     AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenNL     AlexPosn
  | TokenConstr AlexPosn String
  | TokenSig    AlexPosn
  | TokenEquiv AlexPosn
  | TokenHole AlexPosn
  deriving (Eq, Show, Generic)

symString :: Token -> String
symString (TokenSym _ x) = x
symString _ = error "Not a symbol"

constrString :: Token -> String
constrString (TokenConstr _ x) = x

scanTokens = alexScanTokens >>= (return . trim)

trim :: [Token] -> [Token]
trim = reverse . trimNL . reverse . trimNL

trimNL :: [Token] -> [Token]
trimNL [] = []
trimNL (TokenNL _ : ts) = trimNL ts
trimNL ts = ts

instance FirstParameter Token AlexPosn

getPos :: Token -> (Int, Int)
getPos t = (l, c)
  where (AlexPn _ l c) = getFirstParameter t

}
