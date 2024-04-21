-- -*- mode: text; tab-width: 2; -*-
{
module Prop.Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]


tokens :-

  $white+                          ;
  $alpha [$alpha $digit \_ \']*    { \p s -> TokAtom p s }
  \$ $alpha [$alpha $digit \_ \']* { \p s -> TokMetaVar p (drop 1 s) }
  false                            { \p _ -> TokFalse p }
  true                             { \p _ -> TokTrue p }
  \~                               { \p _ -> TokNot p }
  "/\"                             { \p _ -> TokAnd p }
  "\/"                             { \p _ -> TokOr p }
  "==>"                            { \p _ -> TokImp p }
  "<=>"                            { \p _ -> TokIff p }
  \(                               { \p _ -> TokLParen p }
  \)                               { \p _ -> TokRParen p }

{
data Token
  = TokAtom AlexPosn String
  | TokMetaVar AlexPosn String
  | TokFalse AlexPosn
  | TokTrue AlexPosn
  | TokNot AlexPosn
  | TokAnd AlexPosn
  | TokOr AlexPosn
  | TokImp AlexPosn
  | TokIff AlexPosn
  | TokLParen AlexPosn
  | TokRParen AlexPosn
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
