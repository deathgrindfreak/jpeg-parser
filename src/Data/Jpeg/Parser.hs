{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Jpeg.Parser
  ( JpegData (..)
  , parseJpeg
  , parseJpegFile
  , decodeScanData
  )
where

import Control.Exception (throwIO)
import Control.Monad (zipWithM)
import Control.Monad.Loops (unfoldrM)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.Stack (HasCallStack)

import Data.CodeWord
import Data.DCT
import Data.HuffmanTree
import Data.Jpeg.Helper
import Data.Jpeg.Model

import Text.Printf
import Debug.Trace

parseJpegFile :: FilePath -> IO ScanData
parseJpegFile fp = do
  image <- LBS.readFile fp
  case parseOnly parseJpeg image of
    Left err -> throwIO $ JpegParseError err
    Right jpeg -> return jpeg

parseJpeg :: Parser ScanData
parseJpeg = do
  jpegData <- parseJpegData
  parseScanData jpegData

parseJpegData :: Parser JpegData
parseJpegData = do
  imageStartTag
  _ <- many' skipAppHeader
  JpegData
    <$> (concat <$> many1 parseQuantizationTable)
    <*> parseStartOfFrame
    <*> (concat <$> many1 parserHuffmanTree)

skipAppHeader :: Parser ()
skipAppHeader = do
  applicationDefaultHeaderTag
  len <- sectionLength
  skipBytes len

parseQuantizationTable :: Parser [QuantizationTable]
parseQuantizationTable = do
  quantizationTableTag
  len <- sectionLength
  parseLength len $ do
    (p, hdr) <- splitByteInt <$> anyWord8
    let n = 64 * (p + 1)
    table <- V.fromList . map fromIntegral <$> count n anyWord8
    pure (n + 1, QuantizationTable (toEnum hdr) table)

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

parserHuffmanTree :: Parser [HuffmanTree]
parserHuffmanTree = do
  defineHuffmanTreeTag
  len <- sectionLength
  parseLength len $ do
    (t, q) <- splitByteInt <$> anyWord8
    symbolLengths <- count 16 anyWord8

    let n = sum $ map fromIntegral symbolLengths
    symbols <- count n anyWord8

    let hTree =
          HuffmanTree
            (toEnum q)
            (toEnum t)
            (decodeCanonical symbolLengths symbols)
    pure (1 + 16 + n, hTree)

parseScanData :: JpegData -> Parser ScanData
parseScanData jpegData = do
  startOfScanTag
  len <- sectionLength
  skipBytes len
  df <- mkDecodeBuffer <$> takeLazyByteString
  case evalDecoder (decodeScanData jpegData) df of
    Left err -> fail err
    Right r -> pure r

type ScanData = [DCTBlock]

decodeScanData :: JpegData -> Decoder ScanData
decodeScanData jpegData =
  let StartOfFrame _ w h cs = jpegData.startOfFrame
      numBlocks = (w `div` 8) * (h `div` 8)
      dcCoefs = replicate (length cs) 0
   in unfoldrM go (numBlocks, dcCoefs)
  where
    go (0, _) = pure Nothing
    go (n, oldDCCoefs) = do
      Block blocks <- decodeBlock jpegData oldDCCoefs
      let nextCoefs = map (V.head . blockValues) blocks
      pure $ Just (Block $ quantize blocks jpegData, (n - 1, nextCoefs))

    quantize blocks jd =
      let qTable i = (jd.quantizationTables !! i).quantizationTable
       in map
            ( \(BlockComponent c v i) ->
                BlockComponent c (idct $ V.zipWith (*) (qTable i) v) i
            )
            blocks

decodeBlock :: JpegData -> [Int] -> Decoder DecodeBlock
decodeBlock jpegData =
  fmap Block
    . zipWithM (decodeComponent jpegData) jpegData.startOfFrame.components

decodeComponent ::
  JpegData ->
  Component ->
  Int ->
  Decoder DecodeBlockComponent
decodeComponent jpegData (Component cType _ qNum) oldDCCoef = do
  let dcTree = lookupTree DC cType jpegData
      acTree = lookupTree AC cType jpegData

  bfc <- getBuffer
  traceShowM ("before code bf" :: String, bfc)

  code <- fromIntegral <$> decodeCodeWord dcTree
  dcCoef <- (+ oldDCCoef) . signNumber <$> getBits code
  traceShowM (code, dcCoef)

  pairs <- (flip unfoldrM) 1 $ \l ->
    if | l >= 64 -> pure Nothing
       | otherwise -> do
           bf <- getBuffer
           traceShowM ("before" :: String, l, bf)

           (numZeroes, acCode) <- splitByteInt <$> decodeCodeWord acTree

           traceShowM ("" ++ printf "%X" numZeroes, "" ++ printf "%X" acCode)

           let l' = l + numZeroes

           bf' <- getBuffer
           traceShowM ("after" :: String, l', bf')

           if | l' >= 64 -> pure Nothing
              | numZeroes == 15 && acCode == 0 ->
                  pure $ Just ((l', 0), l' + 1)
              | acCode == 0 -> pure Nothing
              | otherwise -> do
                  coef <- signNumber <$> getBits acCode
                  pure $ Just ((l', coef), l' + 1)

  traceShowM pairs

  let vals = V.replicate 64 0 V.// ((0, dcCoef) : pairs)
  pure $ BlockComponent cType vals qNum

signNumber :: CodeWord -> Int
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
    Just (HuffmanTree _ _ t) -> t
  where
    matchTree (HuffmanTree qt tt _) = tt == tType && qt == getQType cType
