{-# LANGUAGE TupleSections #-}

module Data.HuffmanTree.ByteString
  ( decodeCodeWord
  , getBits
  , nextBit
  , removePadding
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Loops (unfoldrM)
import Data.Bifunctor (first)
import Data.Bits (testBit)
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)

import Data.CodeWord
import Data.HuffmanTree.Model

padding :: LBS.ByteString
padding = LBS.pack [0xFF, 0x00]

removePadding :: Decoder ()
removePadding = do
  DecodeBuffer cw bs <- getBuffer
  when (LBS.take 2 bs == padding) $ do
    let bs' = LBS.cons 0xFF $ LBS.drop 2 bs
    putBuffer $ DecodeBuffer cw bs'

mkPaddingDecoder ::
  (DecodeBuffer -> Either String (a, DecodeBuffer)) ->
  Decoder a
mkPaddingDecoder f = removePadding >> Decoder f

getBits :: Int -> Decoder CodeWord
getBits 0 = return mempty
getBits n = foldr1 (<>) <$> unfoldrM go n
  where
    go 0 = return Nothing
    go i = do
      b <- nextBit
      return $ Just (b, i - 1)

nextBit :: Decoder CodeWord
nextBit = mkPaddingDecoder $ \(DecodeBuffer mbcw bs) ->
  case (mbcw, LBS.uncons bs) of
    (Just cw, _) ->
      let (l, r) = splitBit cw
       in Right (l, DecodeBuffer r bs)
    (Nothing, Nothing) -> Left "End of input"
    (Nothing, Just (b, bs')) ->
      let (l, r) = splitBit (mkCodeWordFromBits b)
       in Right (l, DecodeBuffer r bs')

decodeCodeWord :: HTree Word8 -> Decoder Word8
decodeCodeWord t = mkPaddingDecoder $ \df -> do
  (cw, rest) <- unconsBuffer df
  case match t cw of
    Match s cw' -> pure (s, DecodeBuffer cw' rest)
    Continue t' -> runDecoder (decodeCodeWord t') (mkDecodeBuffer rest)
  where
    unconsBuffer (DecodeBuffer mbcw bs) =
      maybe (Left "End of input") Right $
        ((,bs) <$> mbcw) <|> (first mkCodeWordFromBits <$> LBS.uncons bs)

data Match symbol match
  = Match symbol match
  | Continue (HTree symbol)
  deriving (Show)

match ::
  HTree symbol ->
  CodeWord ->
  Match symbol (Maybe CodeWord)
match t (CodeWord cl w) = go t cl
  where
    remainder i = if i == 0 then Nothing else Just $ CodeWord i w

    go Nil _ = error "no match for code word"
    go (Symbol s) i = Match s (remainder i)
    go tr 0 = Continue tr
    go (Tree l r) i = go (if w `testBit` (i - 1) then r else l) (i - 1)
