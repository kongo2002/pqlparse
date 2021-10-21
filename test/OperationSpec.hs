{-# LANGUAGE OverloadedStrings #-}

module OperationSpec ( spec ) where

import qualified Data.Text.Lazy as TL
import           Test.Hspec

import Data.PQL.Operation
import Data.PQL.Parser
import Data.PQL.Types


spec :: Spec
spec = do
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
