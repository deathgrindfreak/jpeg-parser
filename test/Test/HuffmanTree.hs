module Test.HuffmanTree (test_Huffman) where

import Data.Word (Word8)

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

import Data.HuffmanTree

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
    , THH.testProperty "Encoding and decoding should preserve canonical encoding" $
        HH.property $ do
          encoding <- HH.forAll genCanonical
          encodeCanonical (uncurry decodeCanonical encoding) === encoding
    ]

genCanonical :: HH.Gen ([Word8], [Word8])
genCanonical = do
  ls <- Gen.list (Range.singleton 16) (Gen.word8 Range.linearBounded)
  es <- Gen.list (Range.singleton (sum $ map fromIntegral ls)) (Gen.word8 Range.linearBounded)
  return (ls, es)
