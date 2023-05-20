module Data.HuffmanTree.CodeWord
  ( CodeWord
  , mkCodeWord
  , mkCodeWordFromBits
  , codeWordToTup
  , codeWordToBits
  , codeWordLength
  , zeroCodeWord
  , zeroNum
  , addCodeWords
  , splitCodeWordAt
  , splitBit
  )
where

import Data.Bits (Bits, FiniteBits (..), clearBit, shiftL, shiftR, testBit)

data CodeWord a = CodeWord Int a

codeWordLength :: CodeWord a -> Int
codeWordLength (CodeWord l _) = l

instance Functor CodeWord where
  fmap f (CodeWord l n) = CodeWord l (f n)

instance Bits a => Show (CodeWord a) where
  show (CodeWord l n) =
    "<"
      ++ show l
      ++ ", "
      ++ map (\b -> if n `testBit` b then '1' else '0') [l - 1, l - 2 .. 0]
      ++ ">"

mkCodeWord :: Int -> a -> CodeWord a
mkCodeWord = CodeWord

mkCodeWordFromBits :: FiniteBits a => a -> CodeWord a
mkCodeWordFromBits w = CodeWord (finiteBitSize w) w

codeWordToTup :: FiniteBits a => CodeWord a -> (Int, a)
codeWordToTup cw = (codeWordLength cw, codeWordToBits cw)

codeWordToBits :: FiniteBits a => CodeWord a -> a
codeWordToBits (CodeWord l n) = zeroNum l n

zeroCodeWord :: FiniteBits a => CodeWord a -> CodeWord a
zeroCodeWord (CodeWord l n) = CodeWord l (zeroNum l n)

zeroNum :: FiniteBits a => Int -> a -> a
zeroNum l n = foldr (flip clearBit) n [l .. finiteBitSize n - 1]

splitCodeWordAt :: FiniteBits a => Int -> CodeWord a -> (CodeWord a, CodeWord a)
splitCodeWordAt i (CodeWord l w) =
  (CodeWord i (w `shiftR` (l - i)), CodeWord (l - i) (zeroNum (l - i) w))

splitBit ::
  (FiniteBits a, Integral a, Num b, Num c) =>
  CodeWord a ->
  (CodeWord b, Maybe (CodeWord c))
splitBit cw =
  let (l, r) = splitCodeWordAt 1 cw
      (l', r') = (fromIntegral <$> l, fromIntegral <$> r)
   in (l', if codeWordLength r' == 0 then Nothing else Just r')

addCodeWords ::
  (Integral a, Integral b, Bits c, Integral c) =>
  CodeWord a ->
  CodeWord b ->
  CodeWord c
addCodeWords (CodeWord l1 n1) (CodeWord l2 n2) =
  let n1' = fromIntegral n1
      n2' = fromIntegral n2
   in CodeWord (l1 + l2) (n1' `shiftL` l2 + n2')
