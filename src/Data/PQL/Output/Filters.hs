{-# LANGUAGE OverloadedStrings #-}

module Data.PQL.Output.Filters
  ( collectFilters
  , Filters(..)
  ) where


import           Control.Monad ((>=>))
import qualified Data.Text as T

import Data.PQL.Types


data Filters = Filters
  { fCategories :: [T.Text]
  , fBrands     :: [T.Text]
  , fColors     :: [T.Text]
  }


collectFilters :: Expression -> Maybe Filters
collectFilters (And xs) = collectFilters' xs
collectFilters expr     = collectFilters' [expr]


collectFilters' :: [Expression] -> Maybe Filters
collectFilters' expr
  | isValid   = filters
  | otherwise = Nothing
 where
  collect = collectCategories >=> collectBrands >=> collectColors
  filters = foldl collect (Just empty) expr
  isValid = maybe False (not . isEmpty) filters


collectCategories :: Maybe Filters -> Expression -> Maybe Filters
collectCategories Nothing _ = Nothing
collectCategories fs@(Just filters) expr
  | null (fCategories filters) = Just filters { fCategories = values }
  | null values = fs
  | otherwise = Nothing
 where
  values = collectValues "category" 0 expr


collectBrands :: Maybe Filters -> Expression -> Maybe Filters
collectBrands Nothing _ = Nothing
collectBrands fs@(Just filters) expr
  | null (fBrands filters) = Just filters { fBrands = values }
  | null values = fs
  | otherwise = Nothing
 where
  values = collectValues "brand" 0 expr


collectColors :: Maybe Filters -> Expression -> Maybe Filters
collectColors Nothing _ = Nothing
collectColors fs@(Just filters) expr
  | null (fColors filters) = Just filters { fColors = values }
  | null values = fs
  | otherwise = Nothing
 where
  values = collectValues "colorGroup" 0 expr


collectValues :: T.Text -> Int -> Expression -> [T.Text]
collectValues key level (Cond key' Is (Val value))
  | key == key' && level < 2 = [value]
  | otherwise = []

collectValues key level (Or exs)
  | all (not . null) values = concat values
  | otherwise = []
 where
  nextLevel = level + 1
  values = map (collectValues key nextLevel) exs

collectValues _ _ _ = []


empty :: Filters
empty = Filters [] [] []


isEmpty :: Filters -> Bool
isEmpty (Filters cs bs cols) = null (cs ++ bs ++ cols)
