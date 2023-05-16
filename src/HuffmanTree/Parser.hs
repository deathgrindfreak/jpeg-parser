module HuffmanTree.Parser
  ( huffmanTree
  )
where

import Data.Attoparsec.ByteString.Lazy

import Helper.Parser
import HuffmanTree.CanonicalEncoding
import HuffmanTree.Model

huffmanTree :: Parser HuffmanTree
huffmanTree = do
  defineHuffmanTreeTag
  _len <- sectionLength
  ht <- anyWord8

  symbolLengths <- count 16 anyWord8

  let n = sum $ map fromIntegral symbolLengths
  symbols <- count n anyWord8

  pure $ HuffmanTree ht (decodeCanonical symbolLengths symbols)

defineHuffmanTreeTag :: Parser ()
defineHuffmanTreeTag = tag 0xFFC4
