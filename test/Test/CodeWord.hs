module Test.CodeWord (test_CodeWord) where

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

-- import Data.Bits (finiteBitSize)
import Data.CodeWord

test_CodeWord :: Tasty.TestTree
test_CodeWord =
  Tasty.testGroup
    "CodeWord"
    [ THH.testProperty "mkCodeWordFromBits should be idempotent" $
        HH.property $ do
          b <- HH.forAll $ Gen.int Range.constantBounded
          codeWordToBits (mkCodeWordFromBits b) === b
    , THH.testProperty "Length of CodeWord should be idempotent" $
        HH.property $ do
          b <- HH.forAll $ Gen.int Range.constantBounded
          codeWordToTup (mkCodeWordFromBits b) === (msb b , b)
    -- , THH.testProperty "splitCodeWordAt and addCodeWords should be idempotent" $
    --     HH.property $ do
    --       c <- HH.forAll $ Gen.int Range.constantBounded
    --       let cw = mkCodeWordFromBits c
    --       n <- HH.forAll $ Gen.int (Range.constant 0 (finiteBitSize c))
    --       uncurry addCodeWords (splitCodeWordAt n cw) === cw
    ]
