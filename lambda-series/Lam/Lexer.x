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

tokens :-

  $white*$eol                   { \p s -> TokenNL p }
  $eol+                         { \p s -> TokenNL p }
  $white+                       ;
  "--".*                        ;
  @constr                       { \p s -> TokenConstr p s }
  let                           { \p s -> TokenLet p }
  in                            { \p s -> TokenIn p }
  if                            { \p s -> TokenIf p }
  then                          { \p s -> TokenThen p }
  else                          { \p s -> TokenElse p }
  @int                          { \p s -> TokenInt p $ read s }
  @charLiteral                  { \p s -> TokenCharLiteral p $ read s }
  @stringLiteral                { \p s -> TokenStringLiteral p $ read s }
  @sym				                  { \p s -> TokenSym p s }
  "->"                          { \p s -> TokenArrow p }
  \\                            { \p s -> TokenLambda p }
  \=                            { \p s -> TokenEq p }
  "/="                          { \p s -> TokenNeq p }
  [\+]                          { \p s -> TokenAdd p }
  [\-]                          { \p s -> TokenSub p }
  [\*]                          { \p s -> TokenMul p }
  \(                            { \p s -> TokenLParen p }
  \)                            { \p s -> TokenRParen p }
  \:                            { \p s -> TokenSig p }
  "<"                          { \p s -> TokenLesser p }
  ">"                          { \p s -> TokenGreater p }
  "<="                          { \p s -> TokenLesserEq p }
  ">="                          { \p s -> TokenGreaterEq p }
  "=="                          { \p s -> TokenEquiv p }
  "?"                           { \p _ -> TokenHole p }

{

data Token
  = TokenLet    AlexPosn
  | TokenIn     AlexPosn
  | TokenIf     AlexPosn
  | TokenThen   AlexPosn
  | TokenElse   AlexPosn
  | TokenLambda AlexPosn
  | TokenInt    AlexPosn Int
  | TokenSym    AlexPosn String
  | TokenArrow  AlexPosn
  | TokenEq     AlexPosn
  | TokenNeq     AlexPosn
  | TokenAdd    AlexPosn
  | TokenSub    AlexPosn
  | TokenMul    AlexPosn
  | TokenCharLiteral AlexPosn Char
  | TokenStringLiteral AlexPosn Text
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenNL     AlexPosn
  | TokenConstr AlexPosn String
  | TokenSig    AlexPosn
  | TokenLesser AlexPosn
  | TokenGreater AlexPosn
  | TokenLesserEq AlexPosn
  | TokenGreaterEq AlexPosn
  | TokenEquiv AlexPosn
  | TokenHole AlexPosn
  deriving (Eq, Show, Generic)

symString :: Token -> String
symString (TokenSym _ x) = x

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
