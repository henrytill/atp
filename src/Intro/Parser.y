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

Expression
  : var                       { Var $1 }
  | metavar                   { MetaVar $1 }
  | const                     { Const $1 }
  | Expression '^' Expression { Exp $1 $3 }
  | Expression '*' Expression { Mul $1 $3 }
  | Expression '+' Expression { Add $1 $3 }
  | Expression '-' Expression { Sub $1 $3 }
  | '-' Expression %prec NEG  { Neg $2 }
  | '(' Expression ')'        { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
