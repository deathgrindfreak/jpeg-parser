{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module HuffmanTree.Model
  ( HuffmanTree (..)
  , HTree (..)
  , CodeWord (..)
  , mkCodeWord
  )
where

import Data.Bits (testBit, Bits, FiniteBits(..))
import Data.Word (Word8)
import GHC.Stack (HasCallStack)

data CodeWord a = CodeWord Int a

mkCodeWord :: (Num a, FiniteBits a) => a -> CodeWord a
mkCodeWord w =
  CodeWord (if w == 0 then 0 else finiteBitSize w - countLeadingZeros w - 1) w

instance Bits a => Show (CodeWord a) where
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

data HTree a = Nil | Symbol a | Tree (HTree a) (HTree a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Semigroup (HTree a) where
  (<>) :: HasCallStack => HTree a -> HTree a -> HTree a
  Nil <> t = t
  t <> Nil = t
  Tree l1 r1 <> Tree l2 r2 = Tree (l1 <> l2) (r1 <> r2)
  _ <> _ = error "overlapping huffman codes"

instance Monoid (HTree a) where
  mempty = Nil
