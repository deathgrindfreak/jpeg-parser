{-# LANGUAGE TupleSections #-}

module HuffmanTree.CanonicalEncoding
  ( decodeCanonical
  , encodeCanonical
  )
where

import Data.Bifunctor (first)
import Data.Bits (shiftL, testBit)
import Data.List (foldl', group, sortOn, (\\))
import Data.Word (Word8, Word16)
import Safe

import HuffmanTree.Model

type CanonicalCW = CodeWord Word16

encodeCanonical :: HTree Word8 -> ([Word8], [Word8])
encodeCanonical =
  first (addMissingLengths . map (fromIntegral . codeWordLength))
    . unzip
    . sortCodeName
    . foldMap (: [])
    . pathMap
  where
    codeWordLength (CodeWord l _) = l
    sortCodeName = sortOn (\(CodeWord _ n, _) -> n)

    addMissingLengths :: [Word8] -> [Word8]
    addMissingLengths lst =
      let missing = [1 .. 16] \\ lst
          lens = map (\g -> (head g, length g)) $ group lst
       in map (fromIntegral . snd) . sortOn fst $ map (,0) missing ++ lens

decodeCanonical :: [Word8] -> [Word8] -> HTree Word8
decodeCanonical symLens = mconcat . zipWith codeWordToTree (symbolLengthsToCodes symLens)

pathMap :: HTree a -> HTree (CanonicalCW, a)
pathMap = go (CodeWord 0 0)
  where
    go _ Nil = Nil
    go cw (Symbol a) = Symbol (cw, a)
    go (CodeWord cl n) (Tree l r) =
      Tree
        (go (CodeWord (cl + 1) (n `shiftL` 1)) l)
        (go (CodeWord (cl + 1) (n `shiftL` 1 + 1)) r)

codeWordToTree :: CanonicalCW -> a -> HTree a
codeWordToTree (CodeWord cl n) sym = buildTree cl
  where
    buildTree 0 = Symbol sym
    buildTree h =
      if n `testBit` (h - 1)
        then Tree Nil (buildTree (h - 1))
        else Tree (buildTree (h - 1)) Nil

symbolLengthsToCodes :: [Word8] -> [CanonicalCW]
symbolLengthsToCodes syms =
  let lens = zip [1 ..] syms >>= uncurry (flip (replicate . fromIntegral))
      codeWord = CodeWord (headDef 0 lens) 0
   in reverse . snd $ foldl' go (codeWord, [codeWord]) (tailDef [] lens)
  where
    go (CodeWord l cw, ls) len =
      let code = CodeWord len $ (cw + 1) `shiftL` (len - l)
       in (code, code : ls)
