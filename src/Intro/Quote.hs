module Intro.Quote (intro) where

import Data.Generics.Aliases (extQ)
import Intro.Lexer (AlexPosn (..), lex)
import Intro.Parser (parseIntro)
import Intro.Syntax (Expression (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (dataToExpQ, dataToPatQ)
import Prelude hiding (lex)

getSourcePos :: TH.Q AlexPosn
getSourcePos = f <$> TH.location
  where
    f :: TH.Loc -> AlexPosn
    f loc = uncurry (AlexPn 0) (TH.loc_start loc)

parseExpression :: AlexPosn -> String -> TH.Q Expression
parseExpression pos = return . parseIntro . lex pos

antiExpIntro :: Expression -> Maybe (TH.Q TH.Exp)
antiExpIntro (MetaVar v) = Just (TH.varE (TH.mkName v))
antiExpIntro _ = Nothing

quoteExpIntro :: String -> TH.Q TH.Exp
quoteExpIntro str = do
  pos <- getSourcePos
  expr <- parseExpression pos str
  dataToExpQ (const Nothing `extQ` antiExpIntro) expr

antiPatIntro :: Expression -> Maybe (TH.Q TH.Pat)
antiPatIntro (MetaVar v) = Just (TH.varP (TH.mkName v))
antiPatIntro _ = Nothing

quotePatIntro :: String -> TH.Q TH.Pat
quotePatIntro str = do
  pos <- getSourcePos
  expr <- parseExpression pos str
  dataToPatQ (const Nothing `extQ` antiPatIntro) expr

intro :: QuasiQuoter
intro =
  QuasiQuoter
    { quoteExp = quoteExpIntro,
      quotePat = quotePatIntro,
      quoteType = undefined,
      quoteDec = undefined
    }
