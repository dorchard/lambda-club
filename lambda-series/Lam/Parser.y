{
{-# LANGUAGE ImplicitParams #-}
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
    if    { TokenIf _ }
    then  { TokenThen _ }
    else  { TokenElse _ }
    INT   { TokenInt _ _ }
    VAR    { TokenSym _ _ }
    CONSTR { TokenConstr _ _ }
    CHAR   { TokenCharLiteral _ _ }
    STRING { TokenStringLiteral _ _ }
    '\\'  { TokenLambda _ }
    '->'  { TokenArrow _ }
    '='   { TokenEq _ }
    '=='  { TokenEquiv _ }
    '/='  { TokenNeq _ }
    '+'   { TokenAdd _ }
    '-'   { TokenSub _ }
    '*'   { TokenMul _ }
    '('   { TokenLParen _ }
    ')'   { TokenRParen _ }
    ':'   { TokenSig _ }
    '<'   { TokenLesser _ }
    '>'   { TokenGreater _ }
    '<='  { TokenLesserEq _ }
    '>='  { TokenGreaterEq _ }
    '?'   { TokenHole _ }

%right in
%right '->'
%left ':'
%left '+' '-'
%left '*'
%%

Program :: { Expr (Arith ()) }
  : Defs                                        { $1 }

Defs :: { Expr (Arith ()) }
  : Def NL Defs               { $1 $3 }
  | Expr                      { $1 }

NL :: { () }
  : nl NL                     { }
  | nl                        { }

Def :: { Expr (Arith ()) -> Expr (Arith ()) }
  : VAR '=' Expr { \program -> App (Abs (symString $1) program) $3 }

Expr :: { Expr (Arith ()) }
  : let VAR '=' Expr in Expr
    { App (Abs (symString $2) $6) $4 }

  | '\\' VAR '->' Expr
    { Abs (symString $2) $4 }

  | if Expr then Expr else Expr
    { Ext (Conditional $2 $4 $6) }

  | Form
    { $1 }

Form :: { Expr (Arith ()) }
  : Form '+' Form  { Ext (BinOp "+" $1 $3) }
  | Form '-' Form  { Ext (BinOp "-" $1 $3) }
  | Form '*' Form  { Ext (BinOp "*" $1 $3) }
  | Form '<' Form  { Ext (BinOp "<" $1 $3) }
  | Form '>' Form  { Ext (BinOp ">" $1 $3) }
  | Form '<=' Form { Ext (BinOp "<=" $1 $3) }
  | Form '>=' Form { Ext (BinOp ">=" $1 $3) }
  | Form '==' Form { Ext (BinOp "==" $1 $3) }
  | Form '/=' Form { Ext (BinOp "/=" $1 $3) }
  | Juxt           { $1 }

Juxt :: { Expr (Arith ()) }
  : Juxt Atom                 { App $1 $2 }
  | Atom                      { $1 }

Atom :: { Expr (Arith ()) }
  : '(' Expr ')'              { $2 }
  | INT                       { let (TokenInt _ x) = $1
                                 in  Ext (Constant x) }
  | VAR                       { Var (symString $1) }

  -- For later
  -- | '?' { Hole }

{

parseError :: [Token] -> ReaderT String (Either String) a
parseError [] = lift $ Left "Premature end of file"
parseError t  =  do
    file <- ask
    lift . Left $ file <> ":" <> show l <> ":" <> show c <> ": parse error"
  where (l, c) = getPos (head t)

parseProgram :: FilePath -> String -> Either String (Expr (Arith ()))
parseProgram file input = runReaderT (program $ scanTokens input) file

failWithMsg :: String -> IO a
failWithMsg msg = putStrLn msg >> exitFailure

}
