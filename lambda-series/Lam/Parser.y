{
{-# LANGUAGE FlexibleContexts #-}

module Lam.Parser where

import Numeric
import System.Exit
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

import Lam.Lexer
import Lam.Syntax

}

%name program Program
%name expr Expr
%tokentype { Token }
%error { parseError }
%monad { ReaderT String (Either String) }

%token
    nl    { TokenNL _ }
    let   { TokenLet _ }
    in    { TokenIn  _  }
    zero  { TokenZero _ }
    succ  { TokenSucc _ }
    VAR    { TokenSym _ _ }
    LANG   { TokenLang _ _ }
    CONSTR { TokenConstr _ _ }
    '\\'  { TokenLambda _ }
    '->'  { TokenArrow _ }
    '='   { TokenEq _ }
    '('   { TokenLParen _ }
    ')'   { TokenRParen _ }
    ':'   { TokenSig _ }
    '?'   { TokenHole _ }

%right in
%right '->'
%left ':'
%left '+' '-'
%left '*'
%%

Program :: { (Expr PCF, [Option]) }
  : LangOpts Defs  { ($2 $1, $1) }

LangOpts :: { [Option] }
  : LANG nl LangOpts    { (readOption $1) : $3 }
  | {- empty -}         { [] } 

Defs :: { [Option] -> Expr PCF }
  : Def NL Defs           { \opts -> ($1 opts) ($3 opts) }
  | Expr                  { \opts -> $1 opts }

NL :: { () }
  : nl NL                     { }
  | nl                        { }

Def :: { [Option] -> Expr PCF -> Expr PCF }
  : VAR '=' Expr { \opts -> \program -> App (Abs (symString $1) program) ($3 opts) }

Expr :: { [Option] -> Expr PCF }
  : let VAR '=' Expr in Expr
    { \opts -> App (Abs (symString $2) ($6 opts)) ($4 opts) }

  | '\\' VAR '->' Expr
    { \opts -> Abs (symString $2) ($4 opts) }

  | succ '(' Expr ')'
    { \opts ->
        if isPCF opts
          then Ext (Succ ($3 opts))
          else App (Var "succ") ($3 opts) }

  | Expr ':' Type  { \opts -> Sig ($1 opts) $3 }

  | Juxt
    { $1 }

Type :: { Type }
Type
  : CONSTR           { if constrString $1 == "Nat" then NatTy else error "What?" }
  | Type '->' Type   { FunTy $1 $3 }
  | '(' Type ')'     { $2 }

Juxt :: { [Option] -> Expr PCF }
  : Juxt Atom                 { \opts -> App ($1 opts) ($2 opts) }
  | Atom                      { $1 }

Atom :: { [Option] -> Expr PCF }
  : '(' Expr ')'              { $2 }
  | VAR                       { \_ -> Var (symString $1) }
  | zero
     {\opts ->
          if isPCF opts
            then Ext Zero
            else Var "zero" }

  -- For later
  -- | '?' { Hole }

{

readOption :: Token -> Option
readOption (TokenLang _ x) | x == "lang.pcf"   = PCF
readOption (TokenLang _ x) | x == "lang.typed" = Typed
readOption (TokenLang _ x) = error $ "Unknown language option: " <> x
readOption _ = error "Wrong token for language"

parseError :: [Token] -> ReaderT String (Either String) a
parseError [] = lift $ Left "Premature end of file"
parseError t  =  do
    file <- ask
    lift . Left $ file <> ":" <> show l <> ":" <> show c
                        <> ": parse error"
  where (l, c) = getPos (head t)

parseProgram :: FilePath -> String -> Either String (Expr PCF, [Option])
parseProgram file input = runReaderT (program $ scanTokens input) file

failWithMsg :: String -> IO a
failWithMsg msg = putStrLn msg >> exitFailure

}
