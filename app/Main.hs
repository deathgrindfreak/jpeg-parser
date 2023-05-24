module Main (main) where

import Data.Jpeg
import Data.PGM

main :: IO ()
main = do
  jpeg <- parseJpegFile "./image/test_greyscale.jpeg"
  print $ jpegToPGM jpeg
