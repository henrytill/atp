{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Maybe (fromJust)
import PropLogic.Quote (prop)
import PropLogic.Semantics qualified as Semantics
import System.FilePath (takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.PrettyPrint (render)

appendNewline :: ByteString -> ByteString
appendNewline = flip Char8.append (Char8.pack "\n")

tests :: [(String, IO ByteString)]
tests =
  [ ("truthtable", return . appendNewline . Char8.pack . render . Semantics.printTruthtable $ [prop| p ==> q ==> r |])
  ]

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest outFile = goldenVsString name outFile test
  where
    name = takeBaseName outFile
    test = fromJust $ lookup name tests

goldenTests :: IO TestTree
goldenTests = do
  outFiles <- findByExtension [".out"] "test-golden"
  goldens <- mapM (return . mkGoldenTest) outFiles
  return $ testGroup "Golden tests" goldens

main :: IO ()
main = defaultMain =<< goldenTests
