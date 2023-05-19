module Data.HuffmanTree.CodeWord
  ( mkCodeWord
  , codeWordToBits
  , zeroCodeWord
  , zeroNum
  , addCodeWords
  , splitCodeWordAt
  )
where

import Data.Bits (Bits, FiniteBits (..), clearBit, shiftL, shiftR)

import Data.HuffmanTree.Model

mkCodeWord :: FiniteBits a => a -> CodeWord a
mkCodeWord w = CodeWord (finiteBitSize w) w

codeWordToBits :: FiniteBits a => CodeWord a -> a
codeWordToBits (CodeWord l n) = zeroNum l n

zeroCodeWord :: FiniteBits a => CodeWord a -> CodeWord a
zeroCodeWord (CodeWord l n) = CodeWord l (zeroNum l n)

zeroNum :: FiniteBits a => Int -> a -> a
zeroNum l n = foldr (flip clearBit) n [l .. finiteBitSize n - 1]

splitCodeWordAt :: FiniteBits a => Int -> CodeWord a -> (CodeWord a, CodeWord a)
splitCodeWordAt i (CodeWord l w) =
  (CodeWord i (w `shiftR` (l - i)), CodeWord (l - i) (zeroNum (l - i) w))

addCodeWords ::
  (Integral a, Integral b, Bits c, Integral c) =>
  CodeWord a ->
  CodeWord b ->
  CodeWord c
addCodeWords (CodeWord l1 n1) (CodeWord l2 n2) =
  let n1' = fromIntegral n1
      n2' = fromIntegral n2
   in CodeWord (l1 + l2) (n1' `shiftL` l2 + n2')
