module Main (main) where

import Data.Jpeg

main :: IO ()
main = do
  -- jpeg <- parseJpegFile "./image/profile.jpg"
  jpeg <- parseJpegFile "./image/smol.jpg"
  print jpeg
