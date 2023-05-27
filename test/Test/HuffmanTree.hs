module Test.HuffmanTree (test_Huffman) where

import Hedgehog ((===))
import qualified Hedgehog as HH
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
    ]
