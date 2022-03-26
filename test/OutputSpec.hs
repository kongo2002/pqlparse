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

  describe "formatFilters" $ do
    it "nothing" $
      filters' "foo is 9" `shouldBe` Left "failed"

    it "category" $
      filters' "category is 'cat1'" `shouldBe` Right "cat1,,"

    it "multiple categories" $
      filters' "category is 'cat1' or category is 'cat2'" `shouldBe` Right "cat1;cat2,,"

    it "brand" $
      filters' "brand is 'BOSS'" `shouldBe` Right ",BOSS,"

    it "color group" $
      filters' "colorGroup is 'rot'" `shouldBe` Right ",,rot"

    it "category + brand + color group" $
      filters' "brand is 'BOSS' and colorGroup is 'rot' and category is 'cat99'" `shouldBe` Right "cat99,BOSS,rot"

    it "categories + brand + color group" $
      filters' "brand is 'BOSS' and colorGroup is 'rot' and (category is 'cat99' or category is 'cat100')" `shouldBe` Right "cat100;cat99,BOSS,rot"

    it "invalid OR" $
      filters' "colorGroup is 'rot' or category is 'cat99'" `shouldBe` Left "failed"


csv' :: TL.Text -> Either String TL.Text
csv' input = formatCSV `fmap` parse input


json' :: TL.Text -> Either String TL.Text
json' input = formatJSON `fmap` parse input


filters' :: TL.Text -> Either String TL.Text
filters' input =
  let extract value = maybe (Left "failed") Right (formatFilters value)
  in  extract =<< parse input
