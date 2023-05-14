{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module HuffmanTree
  ( HuffmanTree (..)
  , HTree (..)
  , huffmanTree
  , mkTree
  )
where

import Data.Attoparsec.ByteString.Lazy
import Data.Bits (shiftL, testBit)
import Data.List (foldl')
import Data.Word (Word8)
import GHC.Stack (HasCallStack)

import Helper.Parser

data CodeWord = CodeWord Int Int

instance Show CodeWord where
  show (CodeWord l n) =
    "<"
      ++ show l
      ++ ", "
      ++ map (\b -> if n `testBit` b then '1' else '0') [l - 1, l - 2 .. 0]
      ++ ">"

data HuffmanTree = HuffmanTree
  { info :: Word8
  , tree :: HTree Word8
  }
  deriving (Show)

data HTree a = Nil | Leaf a | Tree (HTree a) (HTree a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

empty :: HTree a
empty = Tree Nil Nil

huffmanTree :: Parser HuffmanTree
huffmanTree = do
  defineHuffmanTreeTag
  _len <- sectionLength
  ht <- anyWord8

  symbolLengths <- count 16 anyWord8
  -- when (sum symbolLengths > 256) $
  --   fail "Number of symbols cannot exceed 256"

  let n = sum symbolLengths
  symbols <- count (fromIntegral n) anyWord8

  pure $ HuffmanTree ht (mkTree symbols symbolLengths)

mkTree :: [Word8] -> [Word8] -> HTree Word8
mkTree syms = mconcat . zipWith codeWordToTree syms . symbolLengthsToCodes

instance Semigroup (HTree a) where
  (<>) :: HasCallStack => HTree a -> HTree a -> HTree a
  Nil <> t = t
  t <> Nil = t
  Tree l1 r1 <> Tree l2 r2 = Tree (l1 <> l2) (r1 <> r2)
  _ <> _ = error "overlapping huffman codes"

instance Monoid (HTree a) where
  mempty = Nil

codeWordToTree :: a -> CodeWord -> HTree a
codeWordToTree sym (CodeWord cl n) = addToTree empty cl
  where
    addToTree (Leaf _) _ = error "leaf node in tree build"
    addToTree _ 0 = Leaf sym
    addToTree Nil h = addToTree empty h
    addToTree (Tree l r) h =
      if n `testBit` (h - 1)
        then Tree l (addToTree r (h - 1))
        else Tree (addToTree l (h - 1)) r

symbolLengthsToCodes :: [Word8] -> [CodeWord]
symbolLengthsToCodes syms =
  let lens = zip [1 ..] syms >>= uncurry (flip (replicate . fromIntegral))
      codeWord = CodeWord (head lens) 0
   in reverse . snd $ foldl' go (codeWord, [codeWord]) (tail lens)
  where
    go (CodeWord l cw, ls) len =
      let code = CodeWord len $ (cw + 1) `shiftL` (len - l)
       in (code, code : ls)

defineHuffmanTreeTag :: Parser ()
defineHuffmanTreeTag = tag 0xFFC4
