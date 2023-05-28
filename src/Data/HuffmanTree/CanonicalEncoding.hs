{-# LANGUAGE TupleSections #-}

module Data.HuffmanTree.CanonicalEncoding
  ( decodeCanonical
  , encodeCanonical
  )
where

import Data.Bifunctor (first)
import Data.Bits (shiftL, testBit)
import Data.List (group, sortOn, (\\))
import Data.Traversable (mapAccumL)
import Data.Word (Word8)

import Data.CodeWord
import Data.HuffmanTree.Model

encodeCanonical :: HTree Word8 -> ([Word8], [Word8])
encodeCanonical =
  first (addMissingLengths . map (fromIntegral . codeWordLength))
    . unzip
    . sortCodeName
    . flattenTree
  where
    sortCodeName = sortOn (\(CodeWord _ cw, _) -> cw)

    addMissingLengths :: [Word8] -> [Word8]
    addMissingLengths lst =
      let missing = [1 .. 16] \\ lst
          lens = map (\g -> (head g, length g)) $ group lst
       in map (fromIntegral . snd) . sortOn fst $ map (,0) missing ++ lens

decodeCanonical :: [Word8] -> [Word8] -> HTree Word8
decodeCanonical symLens = mconcat . zipWith codeWordToTree (symbolLengthsToCodes symLens)

codeWordToTree :: CodeWord -> a -> HTree a
codeWordToTree (CodeWord l c) sym = buildTree l
  where
    buildTree 0 = Symbol sym
    buildTree h =
      if c `testBit` (h - 1)
        then Tree Nil (buildTree (h - 1))
        else Tree (buildTree (h - 1)) Nil

symbolLengthsToCodes :: [Word8] -> [CodeWord]
symbolLengthsToCodes syms =
  snd . mapAccumL go Nothing $ zip [1 ..] syms >>= uncurry (flip (replicate . fromIntegral))
  where
    go cw len =
      case cw of
        Nothing -> let code = CodeWord len 0 in (Just code, code)
        Just (CodeWord l c) ->
          let code = CodeWord len $ (c + 1) `shiftL` (len - l)
           in (Just code, code)
