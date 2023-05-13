module Helper.Parser
  ( tag
  , beInt
  , wordsToBEInt
  , skipBytes
  , sectionLength
  )
where

import Control.Monad (void)
import Data.Attoparsec.ByteString.Lazy
import Data.Word (Word16, Word8)

byteWidth :: Int
byteWidth = 16 ^ (2 :: Int)

tag :: Word16 -> Parser ()
tag w =
  let w' = fromIntegral w
      (fw, sw) = w' `divMod` byteWidth
   in word8 (fromIntegral fw) *> word8 (fromIntegral sw) *> pure ()

beInt :: Int -> Parser Int
beInt n = wordsToBEInt <$> count n anyWord8

wordsToBEInt :: [Word8] -> Int
wordsToBEInt = foldr1 ((+) . (byteWidth *)) . map fromIntegral

skipBytes :: Int -> Parser ()
skipBytes n = void $ count n anyWord8

sectionLength :: Parser Int
sectionLength = subtract 2 <$> beInt 2
