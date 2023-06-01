{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import qualified Data.ByteString.Builder as BSB
import Options.Applicative

import Data.Jpeg
import Data.PGM

data Args = Args
  { from :: String
  , to :: String
  }

parseArgs :: ParserInfo Args
parseArgs =
  info
    (args <**> helper)
    ( fullDesc
        <> header "jpeg-to-pbm - convert a jpeg file to pbm"
    )
  where
    args =
      Args
        <$> argument str (metavar "FROM")
        <*> argument str (metavar "TO")

main :: IO ()
main = do
  Args {from, to} <- execParser parseArgs
  jpeg <- parseJpegFile from
  BSB.writeFile to $ jpegToPGM jpeg
