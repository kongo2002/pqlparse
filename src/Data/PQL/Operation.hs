{-# LANGUAGE OverloadedStrings #-}

module Data.PQL.Operation
  ( simplify
  , explode
  , invert
  ) where


import Data.List     ( partition )

import Data.PQL.Types


invert :: Expression -> Expression
invert (Cond attr op val) = Cond attr (invertOp op) val
invert (And as)           = Or (map invert as)
invert (Or os)            = And (map invert os)


invertOp :: Op -> Op
invertOp Is        = IsNot
invertOp IsNot     = Is
invertOp Greater   = LessTE
invertOp GreaterTE = Less
invertOp Less      = GreaterTE
invertOp LessTE    = Greater


simplify :: Expression -> Expression
simplify expr
  | expr /= expr0 = simplify expr0
  | otherwise     = expr0
 where
  expr0 = simplify0 expr


simplify0 :: Expression -> Expression
simplify0 c@Cond {} = c
simplify0 (And vs) =
  let collect (And as) xs = as ++ xs
      collect x xs        = simplify0 x : xs
  in And $ foldr collect [] vs
simplify0 (Or vs) =
  let collect (Or os) xs = os ++ xs
      collect x xs       = simplify0 x : xs
  in Or $ foldr collect [] vs


-- we want so explode expressions like these:
--
--   (a AND b) OR c
--
-- into:
--
--   (a OR c) AND (b OR c)
explode :: Expression -> Expression
explode expr
  | expr /= expr0 = explode expr0
  | otherwise     = expr0
 where
  expr0 = explode0 expr


explode0 :: Expression -> Expression
explode0 (Or vs)  = explodeOr vs
explode0 (And vs) = And (map explode0 vs)
explode0 expr     = expr


explodeOr :: [Expression] -> Expression
explodeOr =
  explodeOr' . partition isAnd
 where
  isAnd And {} = True
  isAnd _      = False


explodeOr' :: ([Expression], [Expression]) -> Expression
explodeOr' ([], rest) = Or rest
explodeOr' (and:ands, rest) =
  And $ withOrs and
 where
  withOrs (And as) = map withOrs' as
  withOrs'         = Or . (: rest ++ ands)
