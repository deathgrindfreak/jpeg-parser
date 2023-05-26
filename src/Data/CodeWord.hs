module Data.CodeWord
  ( CodeWord
  , mkCodeWord
  , mkCodeWordFromBits
  , codeWordToTup
  , codeWordToBits
  , codeWordLength
  , splitCodeWordAt
  , splitBit
  )
where

import Data.Bits

data CodeWord = CodeWord Int Int
  deriving (Eq)

instance Show CodeWord where
  show (CodeWord l n) =
    "<"
      ++ show l
      ++ ", "
      ++ map (\b -> if n `testBit` b then '1' else '0') [l - 1, l - 2 .. 0]
      ++ ">"

mkCodeWord :: Int -> Int -> CodeWord
mkCodeWord = CodeWord

-- Retain the original size of the word, but convert the internal word to Int
mkCodeWordFromBits :: (FiniteBits a, Integral a) => a -> CodeWord
mkCodeWordFromBits w = CodeWord (finiteBitSize w) (fromIntegral w)

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

instance Semigroup CodeWord where
  CodeWord l1 n1 <> CodeWord l2 n2 =
    CodeWord (l1 + l2) (n1 `shiftL` l2 + n2)

instance Monoid CodeWord where
  mempty = CodeWord 0 0
