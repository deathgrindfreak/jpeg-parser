module Data.Jpeg.Parser
  ( JpegData (..)
  , parseJpeg
  , parseJpegFile
  )
where

import Control.Exception (throwIO)

-- import Control.Monad (forM)
import Data.Word (Word8)
import Data.Attoparsec.ByteString.Lazy
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)

import Data.HuffmanTree
import Data.Jpeg.Helper
import Data.Jpeg.Model
import GHC.Stack (HasCallStack)
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Trans.Class (lift)
import Control.Monad.Loops (whileM)
import Data.HuffmanTree.ByteString (decodeCodeWord)

parseJpegFile :: FilePath -> IO JpegData
parseJpegFile fp = do
  image <- LBS.readFile fp
  case parseOnly parseJpeg image of
    Left err -> throwIO $ JpegParseError err
    Right jpeg -> return jpeg

parseJpeg :: Parser JpegData
parseJpeg = do
  jpegData <- parseJpegData
  parseScanData jpegData
  pure jpegData

parseJpegData :: Parser JpegData
parseJpegData = do
  imageStartTag
  skipAppHeader
  JpegData
    <$> count 2 parseQuantizationTable
    <*> parseStartOfFrame
    <*> many' parserHuffmanTree

skipAppHeader :: Parser ()
skipAppHeader = do
  applicationDefaultHeaderTag
  len <- sectionLength
  skipBytes len

parseQuantizationTable :: Parser QuantizationTable
parseQuantizationTable = do
  quantizationTableTag
  _ <- sectionLength
  nfo <- fromIntegral <$> anyWord8

  let hdr = toEnum $ nfo .&. 0x0F
      p = nfo `shiftR` 4

  table <- count (64 * (p + 1)) anyWord8
  pure $ QuantizationTable hdr table

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

    parseComponent =
      Component
        <$> (toEnum . pred . fromIntegral <$> anyWord8)
        <*> parseSamplingFactors
        <*> (fromIntegral <$> anyWord8)

    parseSamplingFactors = do
      sf <- fromIntegral <$> anyWord8
      pure (sf .&. 0x0F, sf `shiftR` 4)

parserHuffmanTree :: Parser HuffmanTree
parserHuffmanTree = do
  defineHuffmanTreeTag
  _len <- sectionLength

  ht <- anyWord8
  let qType = if ht .&. 0x0F == 0 then Luminance else Chrominance
      tType = if ht `shiftR` 4 == 0 then DC else AC

  symbolLengths <- count 16 anyWord8

  let n = sum $ map fromIntegral symbolLengths
  symbols <- count n anyWord8

  pure $ HuffmanTree qType tType (decodeCanonical symbolLengths symbols)

parseScanData :: JpegData -> Parser ()
parseScanData jpegData = do
  startOfScanTag
  bs <- takeLazyByteString
  let Component cType _ qTableNum = head . components $ startOfFrame jpegData
      dcCoef = 0
  -- buildMatrix cType qTableNum dcCoef jpegData
  pure ()

lookupTree :: HasCallStack => TreeType -> ComponentType -> JpegData -> HTree Word8
lookupTree tType cType jpegData =
  case find matchTree (huffmanTrees jpegData) of
    Nothing -> error "failed to find huffman tree"
    Just (HuffmanTree _ _ tree) -> tree
  where
    matchTree (HuffmanTree qt tt _) = tt == tType && qt == getQType cType

    getQType cType =
      case cType of
        Y -> Luminance
        _ -> Chrominance

buildMatrix cType qTableNum oldDCCoef jpegData = do
  let dcTree = lookupTree DC cType jpegData
      acTree = lookupTree AC cType jpegData

  code <- lift $ fromIntegral <$> decodeCodeWord dcTree
  bits <- lift $ getBits code

  whileM ((< 64) =<< ST.get) $ do
    acCode <- lift . fromIntegral <$> decodeCodeWord acTree

  pure []
