{-# LANGUAGE OverloadedStrings #-}

module OutputSpec ( spec ) where

import qualified Data.Text.Lazy as TL
import           Test.Hspec

import Data.PQL.Output
import Data.PQL.Parser


spec :: Spec
spec = do
  describe "formatCSV" $ do
    it "noop" $
      csv' "foo is 9.0" `shouldBe` Right "foo is 9.0"

    it "whitespace" $
      csv' " foo  is 9 " `shouldBe` Right "foo is 9.0"

    it "order" $
      csv' "foo is 9 and bar is 8" `shouldBe` Right "bar is 8.0 and foo is 9.0"

    it "outer parens" $
      csv' "(foo is 9 and bar is 8)" `shouldBe` Right "bar is 8.0 and foo is 9.0"

    it "parens" $
      csv' "(foo is 9 and (bar is 8 or bar is 9))" `shouldBe` Right "foo is 9.0 and (bar is 8.0 or bar is 9.0)"

  describe "formatJSON" $ do
    it "basic expression" $
      json' "foo is 9" `shouldBe` Right "[{\"attr\":\"foo\",\"value\":\"9.0\",\"compare\":\"=\",\"type\":\"number\",\"children\":[]}]"


csv' :: TL.Text -> Either String TL.Text
csv' input = formatCSV `fmap` parse input


json' :: TL.Text -> Either String TL.Text
json' input = formatJSON `fmap` parse input
