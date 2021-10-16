{-# LANGUAGE OverloadedStrings #-}

module Data.PQL.Output
  ( formatCSV
  ) where


import           Data.List     ( intersperse )
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import Data.PQL.Types


formatCSV :: Expression -> TL.Text
formatCSV = TLB.toLazyText . format' True


format' :: Bool -> Expression -> TLB.Builder
format' _ (Cond attr op val) =
  let attr' = TLB.fromText attr
      op'   = formatOp op
      val'  = formatVal val
  in  attr' <> " " <> op' <> " " <> val'
format' topLevel (And cs) =
  let cs' = intersperse " and " (map (format' False) cs)
  in  wrap topLevel cs'
format' topLevel (Or cs) =
  let cs' = intersperse " or " (map (format' False) cs)
  in  wrap topLevel cs'


wrap :: Bool -> [TLB.Builder] -> TLB.Builder
wrap False parts = "(" <> mconcat parts <> ")"
wrap True parts  = mconcat parts


formatVal (Val txt)  = "'" <> TLB.fromText txt <> "'"
formatVal (Bl True)  = "true"
formatVal (Bl False) = "false"
formatVal (Num num)  = TLB.fromString $ show num


formatOp :: Op -> TLB.Builder
formatOp Greater   = "gt"
formatOp GreaterTE = "gte"
formatOp Less      = "lt"
formatOp LessTE    = "lte"
formatOp Is        = "is"
formatOp IsNot     = "isnot"
