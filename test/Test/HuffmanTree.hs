module Test.HuffmanTree (test_Huffman) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

import Data.HuffmanTree
import Data.Jpeg

test_Huffman :: Tasty.TestTree
test_Huffman =
  Tasty.testGroup
    "Huffman Tree Canonical Encoding"
    [ THH.testProperty "Should decode to proper tree" $
        HH.property $ do
          let ls = [0, 2, 2, 3, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
              es = [5, 6, 3, 4, 2, 7, 8, 1, 0, 9]
              expected =
                Tree
                  (Tree (Symbol 5) (Symbol 6))
                  ( Tree
                      (Tree (Symbol 3) (Symbol 4))
                      ( Tree
                          (Tree (Symbol 2) (Symbol 7))
                          ( Tree
                              (Symbol 8)
                              ( Tree
                                  (Symbol 1)
                                  ( Tree
                                      (Symbol 0)
                                      (Tree (Symbol 9) Nil)
                                  )
                              )
                          )
                      )
                  )

          decodeCanonical ls es === expected
    , THH.testProperty "" $
        HH.property $ do
          let es1 = [0x4, 0x5, 0x3, 0x2, 0x6, 0x1, 0x0, 0x7, 0x8, 0x9, 0xA, 0xB]
              ls1 = [0, 0, 7, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0]
              tree1 = HuffmanTree Luminance DC $ decodeCanonical ls1 es1

              es2 = [0x1, 0x2, 0x3, 0x11, 0x4, 0x0, 0x5, 0x21, 0x12, 0x31, 0x41, 0xF0, 0xFA]
              ls2 = [0, 2, 1, 3, 3, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1]
              tree2 = HuffmanTree Luminance AC $ decodeCanonical ls2 es2

              es3 = [0x1, 0x0, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB]
              ls3 = [0, 2, 2, 3, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0]
              tree3 = HuffmanTree Chrominance DC $ decodeCanonical ls3 es3

              es4 = [0x1, 0x0, 0x2, 0x11, 0x3, 0x4, 0x21, 0x12, 0x31, 0x41, 0xF0, 0xFA]
              ls4 = [0, 2, 2, 1, 2, 3, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1]
              tree4 = HuffmanTree Chrominance AC $ decodeCanonical ls4 es4

              components =
                [ Component Y (0, 0) 0
                , Component Cb (0, 0) 0
                , Component Cr (0, 0) 0
                ]
              sos = StartOfFrame 0 0 0 components

              jpegData = JpegData [] sos [tree1, tree2, tree3, tree4]

              buf = mkDecodeBuffer $ LBS.pack [0xFC, 0xFF, 0x00, 0xE2, 0xAF, 0xEF, 0xF3, 0x15, 0x7F]

              v = V.replicate 64 0 V.// [(0, -512)]

          evalDecoder (buildComponentMatrices jpegData) buf === Right [v, v, v]
    ]

-- genCanonical :: HH.Gen ([Word8], [Word8])
-- genCanonical = do
--   ls <- Gen.list (Range.singleton 16) (Gen.word8 Range.linearBounded)
--   es <- Gen.list (Range.singleton (sum $ map fromIntegral ls)) (Gen.word8 Range.linearBounded)
--   return (ls, es)
