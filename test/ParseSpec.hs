{-# LANGUAGE OverloadedStrings #-}

module ParseSpec ( spec ) where

import qualified Data.Text.Lazy as TL
import           Test.Hspec

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

  describe "simplify" $ do
    it "noop" $
      simplify' "foo is 9" `shouldBe` Right (Cond "foo" Is (Num 9))

    it "unnecessary parens" $
      simplify' "(foo is 9)" `shouldBe` Right (Cond "foo" Is (Num 9))

    it "nested parens" $
      simplify' "((foo is 9))" `shouldBe` Right (Cond "foo" Is (Num 9))

    it "nested ANDs" $
      simplify' "foo is 9 and (foo is 10)" `shouldBe` Right (And [Cond "foo" Is (Num 9), Cond "foo" Is (Num 10)])

    it "deep nested ANDs" $
      simplify' "foo is 9 and (foo is 10 and (foo is 11))" `shouldBe` Right (And [Cond "foo" Is (Num 9), Cond "foo" Is (Num 10), Cond "foo" Is (Num 11)])

    it "nested ORs" $
      simplify' "foo is 9 or (foo is 10)" `shouldBe` Right (Or [Cond "foo" Is (Num 9), Cond "foo" Is (Num 10)])

    it "deep nested ORs" $
      simplify' "foo is 9 or (foo is 10 or (foo is 11))" `shouldBe` Right (Or [Cond "foo" Is (Num 9), Cond "foo" Is (Num 10), Cond "foo" Is (Num 11)])

  describe "explode" $ do
    it "noop" $
      explode' "foo is 9" `shouldBe` Right (Cond "foo" Is (Num 9))

    it "OR of ANDs #1" $
      explode' "foo is 9 or (bar gt 10 and bar lt 20)" `shouldBe` Right (And [Or [Cond "bar" Greater (Num 10), Cond "foo" Is (Num 9)], Or [Cond "bar" Less (Num 20), Cond "foo" Is (Num 9)]])

    it "OR of ANDs #2" $
      explode' "foo is 9 or (bar gt 10 and bar lt 20 and ham gt 30)" `shouldBe` Right (And [Or [Cond "bar" Greater (Num 10), Cond "foo" Is (Num 9)], Or [Cond "bar" Less (Num 20), Cond "foo" Is (Num 9)], Or [Cond "ham" Greater (Num 30), Cond "foo" Is (Num 9)]])


simplify' :: TL.Text -> Either String Expression
simplify' input = simplify `fmap` parse input


explode' :: TL.Text -> Either String Expression
explode' input = explode `fmap` parse input
