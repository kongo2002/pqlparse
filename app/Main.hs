{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad      ( forM_, (>=>) )
import           Data.Maybe         ( fromMaybe, mapMaybe )
import           Data.List          ( intersperse, nub )
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TIO
import           System.IO          ( stderr )
import           System.Environment ( getArgs )

import           Data.PQL.Output ( formatCSV )
import           Data.PQL.Parser
import           Data.PQL.Types


main :: IO ()
main = do
  isResolve <- hasResolve
  lines <- TL.lines <$> TIO.getContents
  if isResolve
  then resolve lines
  else convert lines


convert :: [TL.Text] -> IO ()
convert lines =
  forM_ lines handle
 where
  handle line =
    case parse line of
      Right success ->
        TIO.putStrLn $ formatCSV success
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


resolve :: [TL.Text] -> IO ()
resolve lines = do
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

  output (path, cond) = do
    TIO.putStrLn $ TLB.toLazyText $ mconcat parts
   where
    cond' = simplify' cond
    supported =
      if algoliaSupported cond'
      then "OK"
      else "INVALID"

    parts = intersperse ","
      [ TLB.fromText path
      , TLB.fromLazyText $ format cond
      , supported
      , TLB.fromString $ show $ countExpr cond
      ]


hasResolve :: IO Bool
hasResolve =
  hasResolve' `fmap` getArgs


hasResolve' :: [String] -> Bool
hasResolve' ("-r": _)        = True
hasResolve' ("--resolve": _) = True
hasResolve' _                = False


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


pathName :: T.Text -> T.Text
pathName path =
  case T.unsnoc path of
    Just (part, '/') -> part
    _ -> path


pathAttr :: T.Text
pathAttr = "path"
