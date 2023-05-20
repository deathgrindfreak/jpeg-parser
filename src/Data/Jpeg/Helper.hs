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
applicationDefaultHeaderTag = tag 0xFFE0

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
