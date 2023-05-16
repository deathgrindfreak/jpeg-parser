module Jpeg
  ( Jpeg (..)
  , parseJpeg
  , parseJpegFile
  )
where

import Data.Bits ((.&.))
import Data.Word (Word8)
import Control.Exception (Exception (..), throwIO)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as LBS

import Helper.Parser
import HuffmanTree

data Jpeg = Jpeg
  { quantizationTables :: [QuantTable]
  , huffmanTrees :: [HuffmanTree]
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
  tables <- count 2 quantTable
  startOfFrame
  hts <- many' huffmanTree
  pure $ Jpeg tables hts

skipAppHeader :: Parser ()
skipAppHeader = do
  applicationDefaultHeaderTag
  len <- sectionLength
  skipBytes len

data QuantType = Luminance | Chrominance
  deriving Show

data QuantTable = QuantTable QuantType [Word8]
  deriving (Show)

quantTable :: Parser QuantTable
quantTable = do
  quantizationTableTag
  _len <- sectionLength
  nfo <- anyWord8

  let hdr = if nfo .&. 0x0F == 0 then Luminance else Chrominance
      precision = fromIntegral $ nfo .&. 0xF0

  table <- count (64 * (precision + 1)) anyWord8
  pure $ QuantTable hdr table

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
