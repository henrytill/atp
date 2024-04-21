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
  atom    { TokAtom _ $$ }
  metavar { TokMetaVar _ $$ }
  false   { TokFalse _ }
  true    { TokTrue _ }
  '~'     { TokNot _ }
  and     { TokAnd _ }
  or      { TokOr _ }
  imp     { TokImp _ }
  iff     { TokIff _ }
  '('     { TokLParen _ }
  ')'     { TokRParen _ }


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
