{-# LANGUAGE OverloadedStrings #-}

module ParseSpec ( spec ) where

import Test.Hspec

import Data.PQL.Parser
import Data.PQL.Types


spec :: Spec
spec = do
  describe "parse" $ do

    describe "simple expressions" $ do
      it "empty input" $
        parse "" `shouldBe` Left "Failed reading: empty input"
      it "is" $
        parse "foo is 'bar'" `shouldBe` Right (Cond "foo" Is (Val "bar"))
      it "is number" $
        parse "foo is 24" `shouldBe` Right (Cond "foo" Is (Num 24))
      it "isnot" $
        parse "foo isnot 'bar'" `shouldBe` Right (Cond "foo" IsNot (Val "bar"))
      it "gt" $
        parse "foo gt 25" `shouldBe` Right (Cond "foo" Greater (Num 25))
      it "gte" $
        parse "foo gte 28" `shouldBe` Right (Cond "foo" GreaterTE (Num 28))
      it "lt" $
        parse "foo lt 28" `shouldBe` Right (Cond "foo" Less (Num 28))
      it "lte" $
        parse "foo lte 28" `shouldBe` Right (Cond "foo" LessTE (Num 28))

    describe "flipped operators" $ do
      it "is" $
        parse "'bar' is foo" `shouldBe` Right (Cond "foo" Is (Val "bar"))
      it "gt" $
        parse "9 gt foo" `shouldBe` Right (Cond "foo" LessTE (Num 9))
      it "lte" $
        parse "9 lte foo" `shouldBe` Right (Cond "foo" Greater (Num 9))

    describe "basic boolean expressions" $ do
      it "and" $
        parse "foo is 'a' and foo is 'b'" `shouldBe`
          Right (And [Cond "foo" Is (Val "a"), Cond "foo" Is (Val "b")])
      it "or" $
        parse "foo is 'a' or foo is 'b'" `shouldBe`
          Right (Or [Cond "foo" Is (Val "a"), Cond "foo" Is (Val "b")])

    describe "nested boolean expressions" $ do
      it "or in and" $
        parse "foo is 'a' and (foo is 'b' or foo is 'c')" `shouldBe`
          Right (And [Cond "foo" Is (Val "a"), Or [Cond "foo" Is (Val "b"), Cond "foo" Is (Val "c")]])
      it "and in or" $
        parse "foo is 'a' or (foo is 'b' and foo is 'c')" `shouldBe`
          Right (Or [Cond "foo" Is (Val "a"), And [Cond "foo" Is (Val "b"), Cond "foo" Is (Val "c")]])

    describe "simplify expressions" $ do
      it "parenthesis" $
        parse "((foo lte 28))" `shouldBe` Right (Cond "foo" LessTE (Num 28))

    describe "reject invalid expressions" $ do
      it "mixed and/or" $
        parse "foo is 'a' and foo is 'b' or foo is 'c'" `shouldBe` Left "Failed reading: different operators and/or in same group"
