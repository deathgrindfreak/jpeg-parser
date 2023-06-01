{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Options.Applicative

import Data.Jpeg

newtype Args = Args
  { filePath :: String
  }

parseArgs :: ParserInfo Args
parseArgs =
  info
    (args <**> helper)
    ( fullDesc
        <> header "jpeg-info - print header info from a JPEG"
    )
  where
    args = Args <$> argument str (metavar "IMAGE_FILE")

main :: IO ()
main = do
  Args {filePath} <- execParser parseArgs
  jpeg <- parseJpegHeaderInfoFromFile filePath
  putStrLn $ printJpeg jpeg
