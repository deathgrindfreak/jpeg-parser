module Main (main) where

import qualified Data.ByteString.Builder as BSB

import Data.Jpeg
import Data.PGM

main :: IO ()
main = do
  jpeg <- parseJpegFile "./image/test_greyscale.jpeg"
  BSB.writeFile "test_grayscale.pgm" $ jpegToPGM jpeg
