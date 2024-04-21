-- -*- mode: prog; tab-width: 2; -*-
{
module Prop.Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]


tokens :-

  $white+                          ;
  $alpha [$alpha $digit \_ \']*    { \p s -> TokenAtom p s }
  \$ $alpha [$alpha $digit \_ \']* { \p s -> TokenMetaVar p (drop 1 s) }
  false                            { \p _ -> TokenFalse p }
  true                             { \p _ -> TokenTrue p }
  \~                               { \p _ -> TokenNot p }
  "/\"                             { \p _ -> TokenAnd p }
  "\/"                             { \p _ -> TokenOr p }
  "==>"                            { \p _ -> TokenImp p }
  "<=>"                            { \p _ -> TokenIff p }
  \(                               { \p _ -> TokenLParen p }
  \)                               { \p _ -> TokenRParen p }

{
data Token
  = TokenAtom AlexPosn String
  | TokenMetaVar AlexPosn String
  | TokenFalse AlexPosn
  | TokenTrue AlexPosn
  | TokenNot AlexPosn
  | TokenAnd AlexPosn
  | TokenOr AlexPosn
  | TokenImp AlexPosn
  | TokenIff AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  deriving (Eq, Show)

scanTokens :: AlexPosn -> String -> [Token]
scanTokens startPos s = go (startPos, '\n', [], s)
  where
    go a@(pos, _, _, str) =
      case alexScan a 0 of
        AlexEOF -> []
        AlexError ((AlexPn _ line column), _, _, _) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
        AlexSkip  a' _ -> go a'
        AlexToken a' len act -> act pos (take len str) : go a'

lex :: AlexPosn -> String -> [Token]
lex = scanTokens
}
