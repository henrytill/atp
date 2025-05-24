{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as Char8
import PropLogic.Quote (prop)
import PropLogic.Semantics qualified as Semantics
import PropLogic.Syntax (Formula, Prop)
import System.FilePath (takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden qualified as Golden
import Text.PrettyPrint (render, text, ($$))
import Text.PrettyPrint.HughesPJClass (pPrint)

renderTruthtable :: Formula Prop -> ByteString
renderTruthtable = appendNewline . Char8.pack . render . layout
  where
    blankLine = text mempty
    layout fm = pPrint fm $$ blankLine $$ Semantics.printTruthtable fm
    appendNewline = flip ByteString.append "\n"

tests :: [(String, ByteString)]
tests =
  [ -- Example, p. 36
    ("truthtable-p036-example", renderTruthtable [prop| p ==> q ==> r |])
  , -- Peirce's Law, p. 39
    ("truthtable-p039-peirce", renderTruthtable [prop| ((p ==> q) ==> p) ==> p |])
  , -- A simple contradiction, p. 40
    ("truthtable-p040-contradiction", renderTruthtable [prop| p /\ ~p |])
  , -- Example, p. 56
    ("truthtable-p056-example", renderTruthtable [prop| (p \/ q /\ r) /\ (~p \/ ~r) |])
  ]

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest outFile = Golden.goldenVsStringDiff name cmd outFile test
  where
    name = takeBaseName outFile
    cmd ref new = ["diff", "-u", ref, new]
    test = maybe (error $ "No test matching name: " ++ name) return $ lookup name tests

goldenTests :: IO TestTree
goldenTests = do
  outFiles <- Golden.findByExtension [".out"] "test-golden"
  return $ testGroup "Golden Tests" (fmap mkGoldenTest outFiles)

main :: IO ()
main = defaultMain =<< goldenTests
