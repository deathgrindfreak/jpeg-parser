module Test.HuffmanTree.ByteString (test_HuffmanByteString) where

import qualified Data.ByteString.Lazy as LBS

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

import Data.HuffmanTree
import Data.CodeWord

test_HuffmanByteString :: Tasty.TestTree
test_HuffmanByteString =
  Tasty.testGroup
    "Huffman Tree ByteString functions"
    [ THH.testProperty "nextBit on empty input is an error" $
        HH.property $ do
          evalDecoder nextBit (mkDecodeBuffer LBS.empty) === Left "End of input"
    , THH.testProperty "nextBit on input should produce next bit" $
        HH.property $ do
          let bs = mkDecodeBuffer $ LBS.pack [0xBF]
              decoder = (,,) <$> nextBit <*> nextBit <*> nextBit
          evalDecoder decoder bs === Right (mkCodeWord 1 1, mkCodeWord 1 0, mkCodeWord 1 1)
    , THH.testProperty "getBits on empty input is an error" $
        HH.property $ do
          evalDecoder (getBits 1000) (mkDecodeBuffer LBS.empty) === Left "End of input"
    , THH.testProperty "getBits on input should produce proper words" $
        HH.property $ do
          let bs = mkDecodeBuffer $ LBS.pack [0xBD]
              decoder = (,,) <$> getBits 2 <*> getBits 4 <*> getBits 2
          evalDecoder decoder bs === Right (mkCodeWord 2 2, mkCodeWord 4 15, mkCodeWord 2 1)
    , THH.testProperty "removePadding should remove padding" $
        HH.property $ do
          let bs = mkDecodeBuffer $ LBS.pack [0xFF, 0x00, 0x01, 0x02]
          runDecoder removePadding bs === Right ((), mkDecodeBuffer $ LBS.pack [0xFF, 0x01, 0x02])
    ]
