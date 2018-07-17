module Test.Main where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(Right), isLeft)
import Effect (Effect)
import NameParser (nameParser)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)
import Text.Parsing.StringParser (unParser)

main :: Effect Unit
main = runTest do
  suite "nameParser" do
    let testNameParser str = bimap show _.result $ unParser nameParser {str, pos: 0}
    test "works with valid names" do
      let
        str1 = "[HorribleSubs] BlahTastic - 01 [720p].mkv"
        str2 = "[HorribleSubs] Blah BlahTastic - Whatever - 01 [720p].mkv"
        str3 = "[HorribleSubs] Blah Blah BlahTastic - Legend of Blah - 01 [720p].mkv"
      equal (Right "BlahTastic") (_.name <$> testNameParser str1)
      equal (Right "01") (_.episode <$> testNameParser str1)
      equal (Right "Blah BlahTastic - Whatever") (_.name <$> testNameParser str2)
      equal (Right "01") (_.episode <$> testNameParser str2)
      equal (Right "Blah Blah BlahTastic - Legend of Blah") (_.name <$> testNameParser str3)
      equal (Right "01") (_.episode <$> testNameParser str3)
    test "fails with invalid names" do
      let
        str1 = "[HorribleSuahTastic - 01 [720p].mkv"
        str2 = "[HorribleSubs] Blah BlahTastic - 01-20 [720p].mkv"
        str3 = "[HorribleSubs] Blah Blah BlahTastic - Legend of Blah.mkv"
      equal true <<< isLeft $ testNameParser str1
      equal true <<< isLeft $ testNameParser str2
      equal true <<< isLeft $ testNameParser str3
