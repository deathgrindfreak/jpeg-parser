module Main (main) where

import Jpeg

main :: IO ()
main = do
  jpeg <- parseJpegFile "./image/profile.jpg"
  print jpeg
