{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.Jpeg.Parser
  ( JpegData (..)
  , parseJpegFile
  , parseJpegHeaderInfoFromFile
  , parseJpeg
  , parseJpegData
  , decodeScanData
  )
where

import Control.Exception (throwIO)
import Control.Monad (zipWithM)
import Control.Monad.Loops (unfoldrM)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, sortOn)
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.Stack (HasCallStack)

import Data.CodeWord
import Data.Color
import Data.DCT
import Data.HuffmanTree
import Data.Jpeg.Helper
import Data.Jpeg.Model

parseJpegFile :: FilePath -> IO Jpeg
parseJpegFile fp = do
  image <- LBS.readFile fp
  case parseOnly parseJpeg image of
    Left err -> throwIO $ JpegParseError err
    Right jpeg -> return jpeg

parseJpegHeaderInfoFromFile :: FilePath -> IO JpegData
parseJpegHeaderInfoFromFile fp = do
  image <- LBS.readFile fp
  case parseOnly parseJpegData image of
    Left err -> throwIO $ JpegParseError err
    Right jpeg -> return jpeg

parseJpeg :: Parser Jpeg
parseJpeg = do
  jpegData <- parseJpegData
  scans <- parseScanData jpegData
  pure $ Jpeg jpegData scans

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
  case runDecoder (decodeScanData jpegData) df of
    Left err -> fail err
    Right (r, DecodeBuffer _ bs) ->
      if bs == LBS.pack [0xFF, 0xD9]
        then pure r
        else fail "end of image not found"

decodeScanData :: JpegData -> Decoder ScanData
decodeScanData jpegData =
  let StartOfFrame _ w h cs = jpegData.startOfFrame
      numBlocks = (w /// 8) * (h /// 8)
      dcCoefs = replicate (length cs) 0
   in unfoldrM go (numBlocks, dcCoefs)
  where
    go (0, _) = pure Nothing
    go (n, oldDCCoefs) = do
      block <- decodeBlock jpegData oldDCCoefs
      let nextCoefs = blockToList . fmap (V.head . blockValues) $ block
          quantizedBlock = quantize block jpegData
          colorBlocks = colorConvert quantizedBlock
      pure $ Just (colorBlocks, (n - 1, nextCoefs))

    blockToList b =
      case b of
        ColorBlock y cb cr -> [y, cb, cr]
        GrayScaleBlock g -> [g]

    colorConvert b =
      case b of
        ColorBlock y cb cr -> colorConversionMatrix y cb cr
        GrayScaleBlock g -> grayscaleConversion <$> g

    quantize block jd =
      let qTable i = (jd.quantizationTables !! i).quantizationTable
       in fmap (\(BlockComponent _ vs i) -> idct $ V.zipWith (*) (qTable i) vs) block

decodeBlock :: HasCallStack => JpegData -> [Int] -> Decoder DecodeBlock
decodeBlock jpegData =
  fmap toBlock
    . zipWithM (decodeComponent jpegData) jpegData.startOfFrame.components
  where
    toBlock cmps =
      case sortOn blockComponentType cmps of
        [y, cb, cr] -> ColorBlock y cb cr
        [g] -> GrayScaleBlock g
        _ -> error "unknown block type"

decodeComponent ::
  JpegData ->
  Component ->
  Int ->
  Decoder DecodeBlockComponent
decodeComponent jpegData (Component cType _ qNum) oldDCCoef = do
  let dcTree = lookupTree DC cType jpegData
      acTree = lookupTree AC cType jpegData

  code <- fromIntegral <$> decodeCodeWord dcTree
  dcCoef <- (+ oldDCCoef) . signNumber <$> getBits code

  pairs <- flip unfoldrM 1 $ \l ->
    if l >= 64
      then pure Nothing
      else do
        (numZeroes, acCode) <- splitByteInt <$> decodeCodeWord acTree

        let l' = l + numZeroes
        if
            | l' >= 64 -> pure Nothing
            | numZeroes == 15 && acCode == 0 ->
                pure $ Just ((l', 0), l' + 1)
            | numZeroes == 0 && acCode == 0 -> pure Nothing
            | otherwise -> do
                coef <- signNumber <$> getBits acCode
                pure $ Just ((l', coef), l' + 1)

  let vals = V.replicate 64 0 V.// ((0, dcCoef) : pairs)
  pure $ BlockComponent cType vals qNum

signNumber :: CodeWord -> Int
signNumber (CodeWord cl n)
  | cl == 0 = 0
  | otherwise =
      let l = 2 ^ (cl - 1)
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
