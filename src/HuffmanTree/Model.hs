{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module HuffmanTree.Model
  ( HuffmanTree (..)
  , HTree (..)
  , TreeType (..)
  , CodeWord (..)
  , mkCodeWord
  , codeWordToBits
  , addCodeWords
  )
where

import Data.Bits (Bits, FiniteBits (..), testBit, clearBit, shiftL)
import Data.Word (Word8)
import GHC.Stack (HasCallStack)

data CodeWord a = CodeWord Int a

instance Functor CodeWord where
  fmap f (CodeWord l n) = CodeWord l (f n)

mkCodeWord :: FiniteBits a => a -> CodeWord a
mkCodeWord w = CodeWord (finiteBitSize w) w

codeWordToBits :: FiniteBits a => CodeWord a -> a
codeWordToBits (CodeWord l n) =
  foldr (flip clearBit) n [l..finiteBitSize n-1]

addCodeWords ::
  (Integral a, Integral b, Bits c, Integral c) =>
  CodeWord a ->
  CodeWord b ->
  CodeWord c
addCodeWords (CodeWord l1 n1) (CodeWord l2 n2) =
  let n1' = fromIntegral n1
      n2' = fromIntegral n2
   in CodeWord (l1 + l2) (n1' `shiftL` l2 + n2')

instance Bits a => Show (CodeWord a) where
  show (CodeWord l n) =
    "<"
      ++ show l
      ++ ", "
      ++ map (\b -> if n `testBit` b then '1' else '0') [l - 1, l - 2 .. 0]
      ++ ">"

data TreeType = DC | AC deriving (Show)

data HuffmanTree = HuffmanTree
  { header :: Int
  , treeType :: TreeType
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
