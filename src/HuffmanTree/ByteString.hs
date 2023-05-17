{-# LANGUAGE ScopedTypeVariables #-}

module HuffmanTree.ByteString
  ( decodeCodeWord
  , match
  , testTree
  )
where

import qualified Data.ByteString as BS
import Data.Word (Word8)

import Data.Bits (Bits, testBit)
import GHC.Stack (HasCallStack)

import HuffmanTree.Model

data DecodeBuffer a = DecodeBuffer (Maybe (CodeWord a)) BS.ByteString
  deriving (Show)

mkDecodeBuffer :: BS.ByteString -> DecodeBuffer a
mkDecodeBuffer = DecodeBuffer Nothing

decodeByteString :: HTree a -> BS.ByteString -> [a]
decodeByteString t bs = go (mkDecodeBuffer bs)
  where
    go df =
      case decodeCodeWord t df of
        Nothing -> []
        Just (c, df') -> c : go df'

decodeCodeWord ::
  HTree a ->
  DecodeBuffer Int ->
  Maybe (a, DecodeBuffer Int)
decodeCodeWord t df = do
  (cw, rest) <- unconsBuffer df
  case match t cw of
    Match s cw' -> pure (s, DecodeBuffer cw' rest)
    Continue t' -> decodeCodeWord t' (mkDecodeBuffer rest)
  where
    unconsBuffer (DecodeBuffer mbcw bs) =
      case (mbcw, BS.uncons bs) of
        (Nothing, Nothing) -> Nothing
        (Just cw, Nothing) -> Just (fromIntegral <$> cw, BS.empty)
        (Nothing, Just (c, rest)) -> Just (fromIntegral <$> mkCodeWord c, rest)
        (Just cw, Just (c, rest)) -> Just (cw `addCodeWords` (fromIntegral <$> mkCodeWord c), rest)

data Match symbol match
  = Match symbol match
  | Continue (HTree symbol)
  deriving (Show)

match ::
  (HasCallStack, Bits a) =>
  HTree symbol ->
  CodeWord a ->
  Match symbol (Maybe (CodeWord a))
match t (CodeWord msb w) = go t msb
  where
    ind i = max 0 (i - 1)
    remainder i = if i == 0 then Nothing else Just $ CodeWord i w

    go Nil _ = error "no match for code word"
    go (Symbol s) i = Match s (remainder i)
    go tr@(Tree l r) i
      | i == 0 = Continue tr
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
