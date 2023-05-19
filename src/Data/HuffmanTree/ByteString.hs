{-# LANGUAGE TupleSections #-}

module Data.HuffmanTree.ByteString
  ( decodeCodeWord
  , getBits
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Bits (FiniteBits, testBit)
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word16, Word8)
import GHC.Stack (HasCallStack)

import Data.HuffmanTree.CodeWord
import Data.HuffmanTree.Model

getBits :: HasCallStack => Int -> Decoder Word16 (CodeWord Int)
getBits n = Decoder $ \(DecodeBuffer mbcw bs) ->
  case (mbcw, LBS.uncons bs) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just (b, bs')) ->
      let (l, r) = splitCodeWordAt n (mkCodeWord b)
          (l', r') = (fromIntegral <$> l, fromIntegral <$> r)
       in Just (l', DecodeBuffer (Just r') bs')
    (Just cw@(CodeWord l _), Nothing) ->
      if l == n
        then Just (fromIntegral <$> cw, mkDecodeBuffer LBS.empty)
        else error "buffer not consumed"
    (Just cw, Just (b, bs')) ->
      let (l, r) = splitCodeWordAt n (cw `acw` mkCodeWord b)
       in Just (l, DecodeBuffer (Just $ fromIntegral <$> r) bs')
  where
    acw :: CodeWord Word16 -> CodeWord Word8 -> CodeWord Int
    acw = addCodeWords

decodeCodeWord :: HTree Word8 -> Decoder Word16 Word8
decodeCodeWord t = Decoder $ \df -> do
  (cw, rest) <- unconsBuffer df
  case match t cw of
    Match s cw' -> pure (s, DecodeBuffer cw' rest)
    Continue t' -> runDecoder (decodeCodeWord t') (mkDecodeBuffer rest)
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
