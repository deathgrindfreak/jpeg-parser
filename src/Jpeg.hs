module Jpeg
  ( Jpeg (..)
  , parseJpeg
  , parseJpegFile
  )
where

import Control.Exception (Exception (..), throwIO)
import Data.Attoparsec.ByteString.Lazy
import Data.Bits ((.&.))
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)

import Helper.Parser
import HuffmanTree

data Jpeg = Jpeg
  { quantizationTables :: [QuantizationTable]
  , startOfFrame :: StartOfFrame
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
  Jpeg
    <$> count 2 parseQuantizationTable
    <*> parseStartOfFrame
    <*> many' parserHuffmanTree

skipAppHeader :: Parser ()
skipAppHeader = do
  applicationDefaultHeaderTag
  len <- sectionLength
  skipBytes len

data QuantizationType = Luminance | Chrominance
  deriving (Eq, Ord, Enum, Bounded, Show)

data QuantizationTable = QuantizationTable QuantizationType [Word8]
  deriving (Show)

parseQuantizationTable :: Parser QuantizationTable
parseQuantizationTable = do
  quantizationTableTag
  _ <- sectionLength
  nfo <- fromIntegral <$> anyWord8

  let hdr = toEnum $ nfo .&. 0x0F
      p = nfo .&. 0xF0

  table <- count (64 * (p + 1)) anyWord8
  pure $ QuantizationTable hdr table

data ComponentType = Y | Cb | Cr | I | Q
  deriving (Eq, Ord, Enum, Bounded, Show)

data Component = Component
  { componentType :: ComponentType
  , samplingFactors :: (Int, Int)
  , quantizationTableNumber :: Int
  }
  deriving (Show)

data StartOfFrame = StartOfFrame
  { precision :: Int
  , height :: Int
  , width :: Int
  , components :: [Component]
  }
  deriving (Show)

parseStartOfFrame :: Parser StartOfFrame
parseStartOfFrame = do
  startOfFrameTag
  _ <- sectionLength
  StartOfFrame
    <$> (fromIntegral <$> anyWord8)
    <*> beInt 2
    <*> beInt 2
    <*> parseComponents
  where
    parseComponents = do
      n <- fromIntegral <$> anyWord8
      count n parseComponent

    parseComponent = do
      Component
        <$> (toEnum . pred . fromIntegral <$> anyWord8)
        <*> parseSamplingFactors
        <*> (fromIntegral <$> anyWord8)

    parseSamplingFactors = do
      sf <- fromIntegral <$> anyWord8
      pure (sf .&. 0x0F, sf .&. 0xF0)

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
