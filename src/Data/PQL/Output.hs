{-# LANGUAGE OverloadedStrings #-}

module Data.PQL.Output
  ( formatCSV
  , formatJSON
  ) where


import           Data.List     ( intersperse )
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import Data.PQL.Types


formatJSON :: Expression -> TL.Text
formatJSON expr =
  TLB.toLazyText formatted'
 where
  formatted = formatJSON' True expr
  formatted' = "[" <> formatted <> "]"


formatJSON' :: Bool -> Expression -> TLB.Builder
formatJSON' _ (Cond attr op val) =
  "{" <> obj <> "}"
 where
  obj = mconcat $ intersperse "," parts

  parts =
    [ kv "attr" (TLB.fromText attr)
    , kv "value" (value val)
    , kv "compare" (comp op)
    , kv "type" (type' val)
    ]

  comp Is        = "="
  comp IsNot     = "!="
  comp Greater   = ">"
  comp GreaterTE = ">="
  comp Less      = "<"
  comp LessTE    = "<="

  -- TODO: escaping?
  value (Val v)    = TLB.fromText v
  value (Num d)    = TLB.fromString (show d)
  value (Bl True)  = "true"
  value (Bl False) = "false"

  type' (Val _) = "string"
  type' (Num _) = "number"
  type' (Bl _)  = "bool"

formatJSON' _ (Or vs) =
  "{" <> parts <> "}"
 where
  parts = mconcat $ intersperse ","
    [ kv "attr" ""
    , kv "value" ""
    , kv "compare" ""
    , kv "type" "or"
    , str "children" <> ":" <> "[" <> mconcat (intersperse "," (map (formatJSON' False) vs )) <> "]"
    ]

formatJSON' False (And vs) =
  "{" <> parts <> "}"
 where
  parts = mconcat $ intersperse ","
    [ kv "attr" ""
    , kv "value" ""
    , kv "compare" ""
    , kv "type" "and"
    , str "children" <> ":" <> "[" <> mconcat (intersperse "," (map (formatJSON' False) vs )) <> "]"
    ]

formatJSON' True (And vs) =
  mconcat $ intersperse "," (map (formatJSON' False) vs)


str val = "\"" <> val <> "\""
kv key value = str key <> ":" <> str value


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
