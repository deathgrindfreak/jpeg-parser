module Data.CodeWord
  ( CodeWord
  , mkCodeWord
  , mkCodeWordFromBits
  , codeWordToTup
  , codeWordToBits
  , codeWordLength
  , addCodeWords
  , splitCodeWordAt
  , splitBit
  , msb
  )
where

import Data.Bits

data CodeWord = CodeWord Int Int
  deriving Eq

instance Show CodeWord where
  show (CodeWord l n) =
    "<"
      ++ show l
      ++ ", "
      ++ map (\b -> if n `testBit` b then '1' else '0') [l - 1, l - 2 .. 0]
      ++ ">"

msb :: FiniteBits a => a -> Int
msb n = finiteBitSize n - countLeadingZeros n

mkCodeWord :: Int -> Int -> CodeWord
mkCodeWord = CodeWord

mkCodeWordFromBits :: Integral a =>  a -> CodeWord
mkCodeWordFromBits w = let w' = fromIntegral w in CodeWord (msb w') w'

codeWordToTup :: CodeWord -> (Int, Int)
codeWordToTup cw = (codeWordLength cw, codeWordToBits cw)

codeWordLength :: CodeWord -> Int
codeWordLength (CodeWord l _) = l

codeWordToBits :: CodeWord -> Int
codeWordToBits (CodeWord l n) = truncateNum l n

truncateNum :: (Bits a, Num a) => Int -> a -> a
truncateNum l n = n .&. complement (mask l)

mask :: (Bits a, Num a) => Int -> a
mask l = (complement 0 `shiftR` l) `shiftL` l

splitCodeWordAt :: Int -> CodeWord -> (CodeWord, CodeWord)
splitCodeWordAt i (CodeWord l w) =
  (CodeWord i (w `shiftR` (l - i)), CodeWord (l - i) (truncateNum (l - i) w))

splitBit :: CodeWord -> (CodeWord, Maybe CodeWord)
splitBit cw =
  let (l, r) = splitCodeWordAt 1 cw
   in (l, if codeWordLength r == 0 then Nothing else Just r)

addCodeWords :: CodeWord -> CodeWord -> CodeWord
addCodeWords (CodeWord l1 n1) (CodeWord l2 n2) =
  CodeWord (l1 + l2) (n1 `shiftL` l2 + n2)
