-- -*- mode: prog; tab-width: 2; -*-
{
module Prop.Parser where

import Prop.Lexer
import Prop.Syntax
}

%name parseProp
%tokentype { Token }
%error { parseError }

%token
  atom    { TokenAtom _ $$ }
  metavar { TokenMetaVar _ $$ }
  false   { TokenFalse _ }
  true    { TokenTrue _ }
  '~'     { TokenNot _ }
  and     { TokenAnd _ }
  or      { TokenOr _ }
  imp     { TokenImp _ }
  iff     { TokenIff _ }
  '('     { TokenLParen _ }
  ')'     { TokenRParen _ }


%left 'and' 'or'
%left NOT

%%

Formula : atom                  { FmAtom (MkProp $1) }
        | metavar               { FmMetaVar $1 }
        | false                 { FmFalse }
        | true                  { FmTrue }
        | Formula and Formula   { FmAnd $1 $3 }
        | Formula or Formula    { FmOr $1 $3 }
        | Formula imp Formula   { FmImp $1 $3 }
        | Formula iff Formula   { FmIff $1 $3 }
        | '~' Formula %prec NOT { FmNot $2 }
        | '(' Formula ')'       { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
