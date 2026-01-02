module Spec where

import Test.Tasty

import I18nSpec
import ConfigSpec
import FeedParserSpec
import HtmlGenSpec
import UtilsSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Planet Tests"
  [ i18nTests
  , configTests
  , feedTests
  , htmlTests
  , utilityTests
  ]