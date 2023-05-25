{-# LANGUAGE TypeApplications #-}

module Data.Jpeg.Helper
  ( tag
  , beInt
  , wordsToBEInt
  , skipBytes
  , sectionLength
  , imageStartTag
  , applicationDefaultHeaderTag
  , quantizationTableTag
  , startOfFrameTag
  , defineHuffmanTreeTag
  , startOfScanTag
  , imageEndTag
  , splitByte
  , splitByteInt
  , parseLength
  , (///)
  )
where

import Control.Monad (void)
import Data.Attoparsec.ByteString.Lazy
import Data.Bits (shiftR, (.&.))
import Data.Functor (($>))
import Data.Word (Word16, Word8)

byteWidth :: Int
byteWidth = 16 ^ (2 :: Int)

tag :: Word16 -> Parser ()
tag w =
  let w' = fromIntegral w
      (fw, sw) = w' `divMod` byteWidth
   in word8 (fromIntegral fw) *> word8 (fromIntegral sw) $> ()

parseLength :: Int -> Parser (Int, a) -> Parser [a]
parseLength = go 0
  where
    go l n p
      | l > n = fail "Exceeded length"
      | l == n = return []
      | otherwise = do
          (i, r) <- p
          (r :) <$> go (l + i) n p

beInt :: Int -> Parser Int
beInt n = wordsToBEInt <$> count n anyWord8

wordsToBEInt :: [Word8] -> Int
wordsToBEInt = foldr1 ((+) . (byteWidth *)) . map fromIntegral

splitByte :: Word8 -> (Word8, Word8)
splitByte b = (b `shiftR` 4, b .&. 0x0F)

splitByteInt :: Word8 -> (Int, Int)
splitByteInt b = let (l, r) = splitByte b in (fromIntegral l, fromIntegral r)

skipBytes :: Int -> Parser ()
skipBytes n = void $ count n anyWord8

sectionLength :: Parser Int
sectionLength = subtract 2 <$> beInt 2

imageStartTag :: Parser ()
imageStartTag = tag 0xFFD8

applicationDefaultHeaderTag :: Parser ()
applicationDefaultHeaderTag = do
  _ <- word8 0xFF
  void $ satisfy (`elem` [0xE0 .. 0xEF])

quantizationTableTag :: Parser ()
quantizationTableTag = tag 0xFFDB

startOfFrameTag :: Parser ()
startOfFrameTag = tag 0xFFC0

defineHuffmanTreeTag :: Parser ()
defineHuffmanTreeTag = tag 0xFFC4

startOfScanTag :: Parser ()
startOfScanTag = tag 0xFFDA

imageEndTag :: Parser ()
imageEndTag = tag 0xFFD9

-- Python-style integer division
infixl 7 ///

(///) :: Integral a => a -> a -> a
n /// d = round @Double (fromIntegral n / fromIntegral d)
