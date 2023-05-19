module Data.Jpeg.Parser
  ( JpegData (..)
  , parseJpeg
  , parseJpegFile
  )
where

import Control.Exception (throwIO)

import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Word (Word8)

import Control.Monad.Loops (whileM)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import Data.HuffmanTree
import Data.HuffmanTree.ByteString (decodeCodeWord)
import Data.Jpeg.Helper
import Data.Jpeg.Model
import GHC.Stack (HasCallStack)

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
  let (p, hdr) = splitByte nfo
  table <- count (64 * (p + 1)) anyWord8
  pure $ QuantizationTable (toEnum hdr) table

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
        <*> (splitByte . fromIntegral <$> anyWord8)
        <*> (fromIntegral <$> anyWord8)

parserHuffmanTree :: Parser HuffmanTree
parserHuffmanTree = do
  defineHuffmanTreeTag
  _len <- sectionLength

  ht <- anyWord8
  let (t, q) = splitByte ht

  symbolLengths <- count 16 anyWord8

  let n = sum $ map fromIntegral symbolLengths
  symbols <- count n anyWord8

  pure $ HuffmanTree (toEnum q) (toEnum t) (decodeCanonical symbolLengths symbols)

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

buildMatrix cType qTableNum oldDCCoef jpegData = do
  let dcTree = lookupTree DC cType jpegData
      acTree = lookupTree AC cType jpegData

  code <- lift $ fromIntegral <$> decodeCodeWord dcTree
  bits <- lift $ getBits code

  whileM ((< 64) =<< ST.get) $ do
    acCode <- lift . fromIntegral <$> decodeCodeWord acTree

  pure []
