module Main (main) where

import Jpeg
import HuffmanTree

import Text.Pretty.Simple

main :: IO ()
main = do
  let ls = [0, 2, 2, 3, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      es = [5, 6, 3, 4, 2, 7, 8, 1, 0, 9]

  pPrint $ mkTree es ls

  jpeg <- parseJpegFile "./image/profile.jpg"
  print jpeg
