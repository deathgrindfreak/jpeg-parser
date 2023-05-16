module HuffmanTree.Parser
  ( huffmanTree
  )
where

import Data.Attoparsec.ByteString.Lazy
import Data.Bits (testBit, (.&.))

import Helper.Parser
import HuffmanTree.CanonicalEncoding
import HuffmanTree.Model

huffmanTree :: Parser HuffmanTree
huffmanTree = do
  defineHuffmanTreeTag
  _len <- sectionLength

  ht <- anyWord8
  let hdr = fromIntegral $ ht .&. 0x0F
      tType = if ht `testBit` 4 then AC else DC

  symbolLengths <- count 16 anyWord8

  let n = sum $ map fromIntegral symbolLengths
  symbols <- count n anyWord8

  pure $ HuffmanTree hdr tType (decodeCanonical symbolLengths symbols)

defineHuffmanTreeTag :: Parser ()
defineHuffmanTreeTag = tag 0xFFC4
