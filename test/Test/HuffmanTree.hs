module Test.HuffmanTree (unit_Huffman) where

import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as THU

import HuffmanTree

unit_Huffman :: Tasty.TestTree
unit_Huffman =
  Tasty.testGroup
    "Huffman Tree"
    [ THU.testCase "Should produce proper tree" $ do
        let ls = [0, 2, 2, 3, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            es = [5, 6, 3, 4, 2, 7, 8, 1, 0, 9]
            expected =
              Tree
                (Tree (Leaf 5) (Leaf 6))
                ( Tree
                    (Tree (Leaf 3) (Leaf 4))
                    ( Tree
                        (Tree (Leaf 2) (Leaf 7))
                        ( Tree
                            (Leaf 8)
                            ( Tree
                                (Leaf 1)
                                ( Tree
                                    (Leaf 0)
                                    (Tree (Leaf 9) Nil)
                                )
                            )
                        )
                    )
                )

        mkTree es ls @?= expected
    ]
