-- -*- mode: prog; tab-width: 2; -*-
{
module Prop.Parser where

import Prop.Lexer
import Prop.Syntax qualified as Syntax
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

Formula : atom                  { Syntax.Atom (Syntax.MkProp $1) }
        | metavar               { Syntax.MetaVar $1 }
        | false                 { Syntax.False }
        | true                  { Syntax.True }
        | Formula and Formula   { Syntax.And $1 $3 }
        | Formula or Formula    { Syntax.Or $1 $3 }
        | Formula imp Formula   { Syntax.Imp $1 $3 }
        | Formula iff Formula   { Syntax.Iff $1 $3 }
        | '~' Formula %prec NOT { Syntax.Not $2 }
        | '(' Formula ')'       { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
