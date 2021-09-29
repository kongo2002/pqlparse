{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad      ( forM_, (>=>) )
import           Data.Maybe         ( fromMaybe, mapMaybe )
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import           System.IO          ( stderr )
import           System.Environment ( getArgs )

import           Data.PQL.PQL


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
      Just success ->
        TIO.putStrLn $ format success
      Nothing ->
        TIO.hPutStrLn stderr $  "FAILED: " `TL.append` line


algoliaSupported :: Expression -> Bool
algoliaSupported Cond {} = True
algoliaSupported (And vs) =
  all algoliaSupported vs
algoliaSupported (Or vs) =
  all valid vs
 where
  isAnd (And _) = True
  isAnd _       = False

  valid x = not (isAnd x) && algoliaSupported x


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
    in  resolveUntil original

  -- TODO: brute-force attempt to re-resolve the current expression
  resolveUntil m =
    if resolved /= m
    then resolveUntil resolved
    else resolved
   where
    resolved = M.foldrWithKey' updateResolve m m

  parse' (path, pql) =
    case parse pql of
      Just parsed -> Just (path, parsed)
      Nothing -> Nothing

  output (path, cond) = do
    TI.putStr path
    TI.putStr ","
    TIO.putStr $ format cond
    TI.putStr ","
    TI.putStrLn supported
   where
    supported =
      if algoliaSupported cond
      then "OK"
      else "INVALID"


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
