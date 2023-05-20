{-# LANGUAGE OverloadedRecordDot #-}

module Data.Jpeg.Parser
  ( JpegData (..)
  , parseJpeg
  , parseJpegFile
  , buildComponentMatrices
  )
where

import Control.Exception (throwIO)

import Control.Monad.Loops (whileM, unfoldrM)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Data.Word (Word16, Word8)
import GHC.Stack (HasCallStack)

import Data.HuffmanTree
import Data.Jpeg.Helper
import Data.Jpeg.Model

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
  (p, hdr) <- splitByteInt <$> anyWord8
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
        <*> (splitByteInt <$> anyWord8)
        <*> (fromIntegral <$> anyWord8)

parserHuffmanTree :: Parser HuffmanTree
parserHuffmanTree = do
  defineHuffmanTreeTag
  _len <- sectionLength
  (t, q) <- splitByteInt <$> anyWord8
  symbolLengths <- count 16 anyWord8

  let n = sum $ map fromIntegral symbolLengths
  symbols <- count n anyWord8

  pure $ HuffmanTree (toEnum q) (toEnum t) (decodeCanonical symbolLengths symbols)

parseScanData :: JpegData -> Parser ()
parseScanData jpegData = do
  startOfScanTag
  df <- mkDecodeBuffer <$> takeLazyByteString
  let _blah = evalDecoder (buildComponentMatrices jpegData) df
  pure ()

buildComponentMatrices :: JpegData -> Decoder Word16 [V.Vector Int]
buildComponentMatrices jpegData =
  unfoldrM go (0, jpegData.startOfFrame.components)
  where
    go (_, []) = pure Nothing
    go (oldDCCoef, (Component cType _ _) : rest) = do
      v <- buildMatrix cType oldDCCoef jpegData
      pure $ Just (v, (V.head v, rest))

buildMatrix ::
  ComponentType ->
  Int ->
  JpegData ->
  Decoder Word16 (V.Vector Int)
buildMatrix cType oldDCCoef jpegData = do
  let dcTree = lookupTree DC cType jpegData
      acTree = lookupTree AC cType jpegData

  code <- fromIntegral <$> decodeCodeWord dcTree
  dcCoef <- (+ oldDCCoef) . signNumber <$> getBits code

  pairs <- (`ST.evalStateT` 1) $
    whileM ((< 64) <$> ST.get) $ do
      (l', acCode) <- lift $ splitByteInt <$> decodeCodeWord acTree
      ST.modify (\i -> if acCode == 0 then 64 else i + l')

      l <- ST.get
      if l < 64
        then do
          coef <- lift $ signNumber <$> getBits acCode
          ST.modify succ
          pure $ Just (l, coef)
        else pure Nothing

  pure $ V.replicate 64 0 V.// ((0, dcCoef) : catMaybes pairs)

signNumber :: CodeWord Int -> Int
signNumber cw
  | codeWordLength cw == 0 = 0
  | otherwise =
      let (cl, n) = codeWordToTup cw
          l = 2 ^ (cl - 1)
       in if n >= l then n else n - (2 * l - 1)

lookupTree ::
  HasCallStack =>
  TreeType ->
  ComponentType ->
  JpegData ->
  HTree Word8
lookupTree tType cType jpegData =
  case find matchTree (huffmanTrees jpegData) of
    Nothing -> error "failed to find huffman tree"
    Just (HuffmanTree _ _ tree) -> tree
  where
    matchTree (HuffmanTree qt tt _) = tt == tType && qt == getQType cType
