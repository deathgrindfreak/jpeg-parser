{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.HuffmanTree.ByteString
  ( decodeCodeWord
  , getBits
  , nextBit
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Loops (unfoldrM)
import Data.Bifunctor (first)
import Data.Bits (Bits, testBit)
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word16, Word8)
import GHC.Stack (HasCallStack)

import Data.CodeWord
import Data.HuffmanTree.Model

padding :: LBS.ByteString
padding = LBS.pack [0xFF, 0x00]

removePadding :: Decoder Word16 ()
removePadding = do
  DecodeBuffer cw bs <- getBuffer
  let bs' =
        if LBS.take 2 bs == padding
          then LBS.cons 0xFF $ LBS.drop 2 bs
          else bs
  putBuffer $ DecodeBuffer cw bs'

mkPaddingDecoder ::
  (DecodeBuffer Word16 -> Either String (a, DecodeBuffer Word16)) ->
  Decoder Word16 a
mkPaddingDecoder f = removePadding >> Decoder f

getBits :: Int -> Decoder Word16 (CodeWord Int)
getBits 0 = return $ mkCodeWord 0 0
getBits n = foldr1 addCodeWords <$> unfoldrM go n
  where
    go 0 = return Nothing
    go i = do
      b <- nextBit
      return $ Just (b, i - 1)

nextBit :: Decoder Word16 (CodeWord Int)
nextBit = mkPaddingDecoder $ \(DecodeBuffer mbcw bs) ->
  case (mbcw, LBS.uncons bs) of
    (Just cw, _) ->
      let (l, r) = splitBit @Word16 @Int @Word16 cw
       in Right (l, DecodeBuffer r bs)
    (Nothing, Nothing) -> Left "End of input"
    (Nothing, Just (b, bs')) ->
      let (l, r) = splitBit @Word8 @Int @Word16 (mkCodeWordFromBits b)
       in Right (l, DecodeBuffer r bs')

decodeCodeWord :: HTree Word8 -> Decoder Word16 Word8
decodeCodeWord t = mkPaddingDecoder $ \df -> do
  (cw, rest) <- unconsBuffer df
  case match t cw of
    Match s cw' -> pure (s, DecodeBuffer cw' rest)
    Continue t' -> runDecoder (decodeCodeWord t') (mkDecodeBuffer rest)
  where
    unconsBuffer (DecodeBuffer mbcw bs) =
      maybe (Left "End of input") Right $
        ((,bs) <$> mbcw)
          <|> (first ((fromIntegral <$>) . mkCodeWordFromBits) <$> LBS.uncons bs)

data Match symbol match
  = Match symbol match
  | Continue (HTree symbol)
  deriving (Show)

match ::
  (HasCallStack, Bits a, Num a) =>
  HTree symbol ->
  CodeWord a ->
  Match symbol (Maybe (CodeWord a))
match t cw = go t (codeWordLength cw)
  where
    w = codeWordToBits cw

    remainder i = if i == 0 then Nothing else Just $ mkCodeWord i w

    go Nil _ = error "no match for code word"
    go (Symbol s) i = Match s (remainder i)
    go tr 0 = Continue tr
    go (Tree l r) i = go (if w `testBit` (i - 1) then r else l) (i - 1)
