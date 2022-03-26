{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad      ( forM_, (>=>) )
import           Data.Maybe         ( fromMaybe, mapMaybe )
import           Data.List          ( nub )
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import           System.Console.GetOpt
import           System.Environment ( getArgs )
import           System.Exit        ( exitFailure, exitSuccess )
import           System.IO          ( hPutStrLn, stderr )

import           Data.PQL.Operation
import           Data.PQL.Output
import           Data.PQL.Parser
import           Data.PQL.Types


type Formatter = T.Text -> Expression -> Maybe TL.Text


data Options = Options
  { oOutput       :: Formatter
  , oOutputFormat :: String
  , oResolve      :: Bool
  }


defOptions :: Options
defOptions = Options
  { oOutput       = toCSV
  , oOutputFormat = "csv"
  , oResolve      = False
  }


toCSV :: Formatter
toCSV name expr =
  pure toCSV'
 where
  toCSV'
    | T.null name = formatCSV expr
    | otherwise =
      TL.fromStrict name <> "," <> formatCSV expr


toFilters :: Formatter
toFilters name expr
  | T.null name = formatFilters expr
  | otherwise =
    let toLine formatted = TL.fromStrict name <> "," <> formatted
    in  toLine `fmap` formatFilters expr


toJSON :: Formatter
toJSON name expr =
  pure toJSON'
 where
  toJSON'
    | T.null name = formatJSON expr
    | otherwise =
      "{\"id\":\"" <> TL.fromStrict name <> "\",\"queries\":" <> formatJSON expr <> "}"


options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "o" ["output"]
    (ReqArg (\fmt opt -> return $ opt { oOutputFormat = fmt }) "FORMAT")
    "output format [csv,json,filters]"
  , Option "r" ["resolve"]
    (NoArg (\opt -> return opt { oResolve = True }))
    "resolve path expressions"
  , Option "h" ["help"]
    (NoArg (\_ -> putStr usage >> exitSuccess))
    "show this help"
  ]


usage :: String
usage = usageInfo header options
 where
  header = unlines
    [ "Usage: pqlparse [OPTIONS]"
    , ""
    , "pqlparse parses input from stdin and outputs to stdout"
    ]


parseOpts :: [String] -> IO Options
parseOpts args =
  case getOpt Permute options args of
    -- successful
    (o, ps, []) ->
      foldl (>>=) (return defOptions) o >>= validateOpts
    -- errors
    (_, _, es) -> err $ concat es
 where
  err msg = let msg' = "error: " ++ msg ++ "\n\n" ++ usage
            in  hPutStrLn stderr msg' >> exitFailure

  validateOpts opts
    | fmt == "json" = return $ opts { oOutput = toJSON }
    | fmt == "csv" = return $ opts { oOutput = toCSV }
    | fmt == "filters" = return $ opts { oOutput = toFilters }
    | otherwise = err $ "invalid output: " ++ fmt
   where
    fmt = oOutputFormat opts


main :: IO ()
main = do
  opts <- parseOpts =<< getArgs
  lines <- TL.lines <$> TIO.getContents
  if oResolve opts
  then resolve lines (oOutput opts)
  else convert lines (oOutput opts)


convert :: [TL.Text] -> Formatter -> IO ()
convert lines output =
  forM_ lines handle
 where
  handle line =
    case parse line of
      Right success ->
        printIf output "" success
      Left msg ->
        let err  = TL.pack msg
            err' = "FAILED: " <> err <> ": " <> line
        in  TIO.hPutStrLn stderr err'


algoliaSupported :: Expression -> Bool
algoliaSupported Cond {} = True
algoliaSupported (And vs) =
  all algoliaSupported vs
algoliaSupported (Or vs) =
  all valid vs && sameCondType
 where
  isAnd (And _) = True
  isAnd _       = False

  valid x = not (isAnd x) && algoliaSupported x

  sameCondType =
    let types = nub $ mapMaybe condType vs
    in  length types < 2

  -- OR must not contain different types of filter categories
  -- (e.g. numerical and facets)
  condType (Cond _ _ (Num _)) = Just 1
  condType (Cond _ _ (Val _)) = Just 2
  condType _ = Nothing


countExpr :: Expression -> Int
countExpr Cond {} = 1
countExpr (And as) =
  foldr (\a cnt -> countExpr a + cnt) 0 as
countExpr (Or os) =
  foldr (\o cnt -> countExpr o + cnt) 0 os


resolve :: [TL.Text] -> Formatter -> IO ()
resolve lines format =
  forM_ (M.toList lines') output
 where
  lines' =
    let parts line = TL.split (== ',') line
        extract (path : pql : _) =
          Just (pathName (TL.toStrict path), pql)
        extract _ = Nothing

        convert = extract . parts >=> parse'
        original = M.fromList $ mapMaybe convert lines
        resolved = resolveUntil original
    in  M.map simplify' resolved

  -- TODO: brute-force attempt to re-resolve the current expression
  resolveUntil m =
    if resolved /= m
    then resolveUntil resolved
    else resolved
   where
    resolved = M.foldrWithKey' updateResolve m m

  simplify' expr
    | expr /= expr' = simplify' expr'
    | otherwise     = expr'
   where
    expr' = simplify $ explode expr

  parse' (path, pql) =
    case parse pql of
      Right parsed -> Just (path, parsed)
      _ -> Nothing

  output (path, cond) =
    printIf format path cond


printIf :: Formatter -> T.Text -> Expression -> IO ()
printIf formatter name expr =
  case formatter name expr of
    Just output -> TIO.putStrLn output
    Nothing -> return ()


simplify' expr
  | expr /= expr' = simplify' expr'
  | otherwise     = expr'
 where
  expr' = simplify $ explode expr


updateResolve :: T.Text
              -> Expression
              -> M.Map T.Text Expression
              -> M.Map T.Text Expression
updateResolve path cond m =
  let cond' = tryResolve cond m
  in  M.insert path cond' m


tryResolve :: Expression
           -> M.Map T.Text Expression
           -> Expression
tryResolve x@(Cond attr op (Val v)) m
  | attr == pathAttr && op == Is =
    tryResolve' v x m
  | attr == pathAttr && op == IsNot =
    tryResolveInv' v x m
  | otherwise = x

tryResolve (And vs) m =
  let vs' = map (flip tryResolve m) vs
  in  And vs'

tryResolve (Or vs) m =
  let vs' = map (flip tryResolve m) vs
  in  Or vs'

tryResolve cond _ = cond


tryResolve' :: T.Text -> Expression -> M.Map T.Text Expression -> Expression
tryResolve' path cond m =
  fromMaybe cond $ M.lookup path' m
 where
  path' = pathName path


tryResolveInv' :: T.Text -> Expression -> M.Map T.Text Expression -> Expression
tryResolveInv' path cond m =
  maybe cond invert lookup
 where
  path' = pathName path
  lookup = M.lookup path' m


pathName :: T.Text -> T.Text
pathName path =
  case T.unsnoc path of
    Just (part, '/') -> part
    _ -> path


pathAttr :: T.Text
pathAttr = "path"
