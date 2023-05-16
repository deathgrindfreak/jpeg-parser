module HuffmanTree.ByteString
  ( decodeByteString
  , match
  , testTree
  )
where

import qualified Data.ByteString as BS
import Data.Word (Word8)

import Data.Bits (Bits, testBit)
import GHC.Stack (HasCallStack)

import HuffmanTree.Model

decodeByteString :: HTree Word8 -> BS.ByteString -> BS.ByteString
decodeByteString = undefined

data Match symbol match
  = Match symbol match
  | Continue (HTree symbol) match
  deriving (Show)

match ::
  (HasCallStack, Bits a) =>
  HTree Word8 ->
  CodeWord a ->
  Match Word8 (Maybe (CodeWord a))
match t (CodeWord msb w) = go t msb
  where
    ind i = max 0 (i - 1)
    remainder i = if i == 0 then Nothing else Just $ CodeWord i w

    go Nil _ = error "no match for code word"
    go (Symbol s) i = Match s (remainder i)
    go tr@(Tree l r) i
      | i == 0 = Continue tr (remainder i)
      | otherwise = go (if w `testBit` ind i then r else l) (i - 1)

testTree :: HTree Word8
testTree =
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
