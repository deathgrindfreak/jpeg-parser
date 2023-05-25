module Test.Decode (test_Decode) where

import qualified Data.ByteString.Builder as BSB

import qualified Test.Tasty as Tasty
import Test.Tasty.Golden (goldenVsString)

import Data.Jpeg
import Data.PGM

test_Decode :: Tasty.TestTree
test_Decode =
  Tasty.testGroup
    "JPEG Decode"
    [ goldenVsString
        "test_greyscale.jpeg"
        "test/golden/test_greyscale.pgm"
        ( BSB.toLazyByteString . jpegToPGM
            <$> parseJpegFile "image/test_greyscale.jpeg"
        )
    ]
