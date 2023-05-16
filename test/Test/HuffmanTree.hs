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

        decode ls es @?= expected
    ]
