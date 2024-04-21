-- -*- mode: text; tab-width: 2; -*-
{
module Intro.Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                          ;
  $alpha [$alpha $digit \_ \']*    { \p s -> TokVar p s }
  \$ $alpha [$alpha $digit \_ \']* { \p s -> TokMetaVar p (drop 1 s) }
  $digit+                          { \p s -> TokConst p (read s) }
  \+                               { \p _ -> TokPlus p }
  \-                               { \p _ -> TokMinus p }
  \*                               { \p _ -> TokTimes p }
  \^                               { \p _ -> TokExp p }
  \(                               { \p _ -> TokLParen p }
  \)                               { \p _ -> TokRParen p }

{
data Token
  = TokVar AlexPosn String
  | TokMetaVar AlexPosn String
  | TokConst AlexPosn Integer
  | TokPlus AlexPosn
  | TokMinus AlexPosn
  | TokTimes AlexPosn
  | TokExp AlexPosn
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
