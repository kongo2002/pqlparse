module Data.PQL.Types
  ( Expression(..)
  , Value(..)
  , Op(..)
  ) where


import qualified Data.Text as T


data Expression
  = Cond T.Text Op Value
  | And [Expression]
  | Or [Expression]
  deriving ( Show, Eq )


data Value
  = Val T.Text
  | Num Double
  | Bl Bool
  deriving ( Show, Eq, Ord )


data Op
  = Is
  | IsNot
  | Greater
  | GreaterTE
  | Less
  | LessTE
  deriving ( Show, Eq )
