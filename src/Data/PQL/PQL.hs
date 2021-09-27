{-# LANGUAGE OverloadedStrings #-}

module Data.PQL.PQL
    ( parse
    , format
    , Condition(..)
    , Op(..)
    , Value(..)
    ) where


import           Control.Applicative
import           Control.Monad ( when, unless )
import           Data.Char     ( isAlphaNum )
import           Data.Monoid   ( (<>), mempty, mconcat )
import           Data.Functor  ( ($>) )
import           Data.List     ( intersperse, sortBy )
import qualified Data.Attoparsec.Text.Lazy as AL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB


data Condition
  = Cond T.Text Op Value
  | And [Condition]
  | Or [Condition]
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


format :: Condition -> TL.Text
format = TLB.toLazyText . format'


format' :: Condition -> TLB.Builder
format' (Cond attr op val) =
  let attr' = TLB.fromText attr
      op'   = formatOp op
      val'  = formatVal val
  in  attr' <> " " <> op' <> " " <> val'
format' (And cs) =
  let cs' = intersperse " and " (map format' cs)
  in  "(" <> mconcat cs' <> ")"
format' (Or cs) =
  let cs' = intersperse " or " (map format' cs)
  in  "(" <> mconcat cs' <> ")"


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


flipOp :: Op -> Op
flipOp Greater   = LessTE
flipOp GreaterTE = Less
flipOp Less      = GreaterTE
flipOp LessTE    = Greater
flipOp op        = op


parse :: TL.Text -> Maybe Condition
parse input =
  case AL.parse group input of
    AL.Fail {}    -> Nothing
    AL.Done _ res -> Just res


condition :: AL.Parser Condition
condition =
  (AL.char '(' *> group) <|> parseCond


orderValue :: Condition -> Condition -> Ordering
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


order :: Condition -> Condition
order (And as) = And (sortBy orderValue as)
order (Or as)  = Or (sortBy orderValue as)
order cond     = cond


simplify :: Condition -> Condition
simplify c@Cond {} = c
simplify (And vs) =
  let collect (And as) xs = as ++ xs
      collect x xs        = x : xs
  in And $ foldr collect [] vs
simplify (Or vs) =
  let collect (Or os) xs = os ++ xs
      collect x xs       = x : xs
  in Or $ foldr collect [] vs


group :: AL.Parser Condition
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

  collect :: Maybe Comb -> [Condition] -> AL.Parser (Maybe Comb, [Condition])
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


parseCond :: AL.Parser Condition
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
