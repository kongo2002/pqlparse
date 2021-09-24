{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad ( forM_ )
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import           System.IO     ( stderr )

import           Data.PQL.PQL  ( parse )


main :: IO ()
main = do
  lines <- TL.lines <$> TIO.getContents
  forM_ lines handle
 where
  handle line =
    case parse line of
      Just success ->
        TIO.putStrLn $ TL.pack $ show success
      Nothing ->
        TIO.hPutStrLn stderr $  "FAILED: " `TL.append` line
