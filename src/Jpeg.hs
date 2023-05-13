module Jpeg
  ( Jpeg (..)
  , parseJpeg
  , parseJpegFile
  )
where

import Control.Exception (Exception (..), throwIO)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as LBS

import Helper.Parser
import HuffmanTree

data Jpeg = Jpeg
  { huffmanTrees :: [HuffmanTree]
  }
  deriving (Show)

newtype JpegParseError = JpegParseError String
  deriving (Show)

instance Exception JpegParseError

parseJpegFile :: FilePath -> IO Jpeg
parseJpegFile fp = do
  image <- LBS.readFile fp
  case parseOnly parseJpeg image of
    Left err -> throwIO $ JpegParseError err
    Right jpeg -> return jpeg

parseJpeg :: Parser Jpeg
parseJpeg = do
  imageStartTag
  skipAppHeader
  _ <- count 2 quantTable
  startOfFrame
  hts <- many' huffmanTree
  pure $ Jpeg hts

skipAppHeader :: Parser ()
skipAppHeader = do
  applicationDefaultHeaderTag
  len <- sectionLength
  skipBytes len

data QuantTable = QuantTable

quantTable :: Parser QuantTable
quantTable = do
  quantizationTableTag
  len <- sectionLength
  skipBytes len -- Skip for now
  pure QuantTable

startOfFrame :: Parser ()
startOfFrame = do
  startOfFrameTag
  len <- sectionLength
  skipBytes len

imageStartTag :: Parser ()
imageStartTag = tag 0xFFD8

applicationDefaultHeaderTag :: Parser ()
applicationDefaultHeaderTag = tag 0xFFE0

quantizationTableTag :: Parser ()
quantizationTableTag = tag 0xFFDB

startOfFrameTag :: Parser ()
startOfFrameTag = tag 0xFFC0

-- startOfScanTag :: Parser ()
-- startOfScanTag = tag 0xFFDA

-- imageEndTag :: Parser ()
-- imageEndTag = tag 0xFFD9
