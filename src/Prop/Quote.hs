module Prop.Quote (prop) where

import Data.Generics.Aliases (extQ)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (dataToExpQ, dataToPatQ)
import Prop.Lexer (AlexPosn (..), lex)
import Prop.Parser (parseProp)
import Prop.Syntax (Formula (..), Prop)
import Prelude hiding (lex)

getSourcePos :: TH.Q AlexPosn
getSourcePos = f <$> TH.location
  where
    f :: TH.Loc -> AlexPosn
    f loc = uncurry (AlexPn 0) (TH.loc_start loc)

parseFormula :: AlexPosn -> String -> TH.Q (Formula Prop)
parseFormula pos = return . parseProp . lex pos

antiExpProp :: Formula Prop -> Maybe (TH.Q TH.Exp)
antiExpProp (FmMetaVar v) = Just (TH.varE (TH.mkName v))
antiExpProp _ = Nothing

quoteExpProp :: String -> TH.Q TH.Exp
quoteExpProp str = do
  pos <- getSourcePos
  expr <- parseFormula pos str
  dataToExpQ (const Nothing `extQ` antiExpProp) expr

antiPatProp :: Formula Prop -> Maybe (TH.Q TH.Pat)
antiPatProp (FmMetaVar v) = Just (TH.varP (TH.mkName v))
antiPatProp _ = Nothing

quotePatProp :: String -> TH.Q TH.Pat
quotePatProp str = do
  pos <- getSourcePos
  expr <- parseFormula pos str
  dataToPatQ (const Nothing `extQ` antiPatProp) expr

prop :: QuasiQuoter
prop =
  QuasiQuoter
    { quoteExp = quoteExpProp,
      quotePat = quotePatProp,
      quoteType = undefined,
      quoteDec = undefined
    }
