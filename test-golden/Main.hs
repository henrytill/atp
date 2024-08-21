{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Maybe (fromJust)
import PropLogic.Quote (prop)
import PropLogic.Semantics qualified as Semantics
import PropLogic.Syntax (Formula, Prop)
import System.FilePath (takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Text.PrettyPrint (render)

renderTruthtable :: Formula Prop -> IO ByteString
renderTruthtable = return . appendNewline . Char8.pack . render . Semantics.printTruthtable
  where
    appendNewline = flip ByteString.append "\n"

tests :: [(String, IO ByteString)]
tests =
  [ ("truthtable-example", renderTruthtable [prop| p ==> q ==> r |])
  ]

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest outFile = goldenVsStringDiff name cmd outFile test
  where
    name = takeBaseName outFile
    cmd ref new = ["diff", "-u", ref, new]
    test = fromJust $ lookup name tests

goldenTests :: IO TestTree
goldenTests = do
  outFiles <- findByExtension [".out"] "test-golden"
  return $ testGroup "Golden tests" (mkGoldenTest <$> outFiles)

main :: IO ()
main = defaultMain =<< goldenTests
