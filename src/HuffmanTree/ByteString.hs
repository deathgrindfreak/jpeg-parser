{-# LANGUAGE TupleSections #-}

module HuffmanTree.ByteString
  ( decodeByteString
  )
where

import Data.List (unfoldr)
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8, Word16)
import Data.Bits (FiniteBits, testBit)
import GHC.Stack (HasCallStack)
import Data.Bifunctor (first)
import Control.Applicative ((<|>))

import HuffmanTree.Model

data DecodeBuffer a = DecodeBuffer (Maybe (CodeWord a)) LBS.ByteString
  deriving (Show)

mkDecodeBuffer :: LBS.ByteString -> DecodeBuffer a
mkDecodeBuffer = DecodeBuffer Nothing

decodeByteString :: HTree Word8 -> LBS.ByteString -> [Word8]
decodeByteString t bs = unfoldr (decodeCodeWord t) (mkDecodeBuffer bs)

decodeCodeWord ::
  HTree Word8 ->
  DecodeBuffer Word16 ->
  Maybe (Word8, DecodeBuffer Word16)
decodeCodeWord t df = do
  (cw, rest) <- unconsBuffer df
  case match t cw of
    Match s cw' -> pure (s, DecodeBuffer cw' rest)
    Continue t' -> decodeCodeWord t' (mkDecodeBuffer rest)
  where
    unconsBuffer (DecodeBuffer mbcw bs) =
      ((,bs) <$> mbcw) <|> (first ((fromIntegral <$>) . mkCodeWord) <$> LBS.uncons bs)

data Match symbol match
  = Match symbol match
  | Continue (HTree symbol)
  deriving (Show)

match ::
  (HasCallStack, FiniteBits a) =>
  HTree symbol ->
  CodeWord a ->
  Match symbol (Maybe (CodeWord a))
match t (CodeWord msb w) = go t msb
  where
    remainder i = if i == 0 then Nothing else Just $ CodeWord i w

    go Nil _ = error "no match for code word"
    go (Symbol s) i = Match s (remainder i)
    go tr 0 = Continue tr
    go (Tree l r) i = go (if w `testBit` (i - 1) then r else l) (i - 1)
