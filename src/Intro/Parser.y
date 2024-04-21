-- -*- mode: prog; tab-width: 2; -*-
{
module Intro.Parser where

import Intro.Lexer
import Intro.Syntax
}

%name parseIntro
%tokentype { Token }
%error { parseError }

%token
  var     { TokVar _ $$ }
  metavar { TokMetaVar _ $$ }
  const   { TokConst _ $$ }
  '+'     { TokPlus _ }
  '-'     { TokMinus _ }
  '*'     { TokTimes _ }
  '^'     { TokExp _ }
  '('     { TokLParen _ }
  ')'     { TokRParen _ }

%left '+' '-'
%left '*'
%right '^'
%left NEG

%%

Exp : var               { Var $1 }
    | metavar           { MetaVar $1 }
    | const             { Const $1 }
    | Exp '^' Exp       { Exp $1 $3 }
    | Exp '*' Exp       { Mul $1 $3 }
    | Exp '+' Exp       { Add $1 $3 }
    | Exp '-' Exp       { Sub $1 $3 }
    | '-' Exp %prec NEG { Neg $2 }
    | '(' Exp ')'       { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
