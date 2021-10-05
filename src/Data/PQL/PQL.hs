{-# LANGUAGE OverloadedStrings #-}

module Data.PQL.PQL
    ( parse
    , format
    , explode
    , simplify
    , Expression(..)
    , Op(..)
    , Value(..)
    ) where


import           Control.Applicative
import           Control.Monad ( when, unless )
import           Data.Char     ( isAlphaNum )
import           Data.Monoid   ( (<>), mempty, mconcat )
import           Data.Functor  ( ($>) )
import           Data.List     ( intersperse, partition, sortBy )
import qualified Data.Attoparsec.Text.Lazy as AL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB


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


data Comb = CAnd | COr deriving ( Eq )


format :: Expression -> TL.Text
format = TLB.toLazyText . format' True


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


-- we want so explode expressions like these:
--
--   (a AND b) OR c
--
-- into:
--
--   (a OR c) AND (b OR c)
explode0 :: Expression -> Expression
explode0 (Or vs)  = explodeOr vs
explode0 (And vs) = And (map explode0 vs)
explode0 expr     = expr


explode :: Expression -> Expression
explode expr
  | expr /= expr0 = explode expr0
  | otherwise     = expr0
 where
  expr0 = explode0 expr


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


flipOp :: Op -> Op
flipOp Greater   = LessTE
flipOp GreaterTE = Less
flipOp Less      = GreaterTE
flipOp LessTE    = Greater
flipOp op        = op


parse :: TL.Text -> Either String Expression
parse input =
  AL.eitherResult $ AL.parse group input


condition :: AL.Parser Expression
condition =
  parseCond' <|> ((AL.char '(' AL.<?> "expecting condition") *> group)
 where
  -- we are trying to parse the condition like this
  -- before the group for a better error message when
  -- `group` fails with invalid condition groups
  parseCond' = do
    chr <- AL.peekChar'
    when (chr == '(') $ fail "group expected"
    parseCond


orderValue :: Expression -> Expression -> Ordering
orderValue (Cond a1 _ v1) (Cond a2 _ v2) =
  mconcat [compare a1 a2, compare v1 v2]
orderValue Cond {} _      = LT
orderValue _ Cond {}      = GT
orderValue (And _) (Or _) = LT
orderValue (Or _) (And _) = GT
orderValue (And as) (And as') =
  compare (length as) (length as')
orderValue (Or os) (Or os') =
  compare (length os) (length os')


order :: Expression -> Expression
order (And as) = And (sortBy orderValue as)
order (Or as)  = Or (sortBy orderValue as)
order cond     = cond


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


simplify :: Expression -> Expression
simplify expr
  | expr /= expr0 = simplify expr0
  | otherwise     = expr0
 where
  expr0 = simplify0 expr


group :: AL.Parser Expression
group = do
  (comb, cs) <- collect Nothing []
  case comb of
    Nothing ->
      if null cs
      then fail "empty input"
      else return $ head cs
    Just CAnd -> return $ prepare $ And cs
    Just COr -> return $ prepare $ Or cs
 where
  prepare = order . simplify

  collect :: Maybe Comb -> [Expression] -> AL.Parser (Maybe Comb, [Expression])
  collect op cs = do
    AL.skipSpace
    atEnd <- isEnd
    if atEnd
    then return (op, cs)
    else do
      c <- condition
      AL.skipSpace
      ifClose <- isEnd
      if ifClose
      then do
        return (op, cs ++ [c])
      else do
        op' <- ops
        when (invalid op') $ fail "different operators and/or in same group"
        collect (Just op') (cs ++ [c])

   where
    isEnd = AL.option False ((True <$ AL.char ')') <|> (True <$ AL.endOfInput))
    invalid operator =
      case op of
        Just o -> o /= operator
        Nothing -> False

  ops :: AL.Parser Comb
  ops = ("and" $> CAnd) <|> ("or" $> COr)


parseCond :: AL.Parser Expression
parseCond =
  cond' <|> cond''
 where
  attribute = AL.takeWhile1 isAlphaNum

  cond' = do
    val <- parseValue
    _ <- AL.skipSpace
    op <- flipOp <$> parseOp
    _ <- AL.skipSpace
    attr <- attribute
    return $ Cond attr op val

  cond'' = do
    attr <- attribute
    _ <- AL.skipSpace
    op <- parseOp
    _ <- AL.skipSpace
    val <- parseValue
    return $ Cond attr op val


parseValue :: AL.Parser Value
parseValue =
  number <|> bool <|> value
 where
  number = Num <$> AL.double
  bool   = (Bl True <$ "true") <|> (Bl False <$ "false")
  -- TODO: escape quotes
  value  = Val <$> (AL.char '\'' *> AL.takeWhile1 (/= '\'') <* AL.char '\'')


parseOp :: AL.Parser Op
parseOp =
  ("isnot" $> IsNot) <|>
  ("is" $> Is) <|>
  ("gte" $> GreaterTE) <|>
  ("gt" $> Greater) <|>
  ("lte" $> LessTE) <|>
  ("lt" $> Less)
