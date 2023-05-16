{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module HuffmanTree
  ( HuffmanTree (..)
  , HTree (..)
  , huffmanTree
  , decodeCanonical
  , encodeCanonical
  )
where

import Data.Attoparsec.ByteString.Lazy
import Data.Bits (shiftL, testBit)
import Data.List (foldl', group, sortOn, (\\))
import Data.Word (Word8)
import GHC.Stack (HasCallStack)
import Data.Bifunctor (first)

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

data HTree a = Nil | Symbol a | Tree (HTree a) (HTree a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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

  pure $ HuffmanTree ht (decodeCanonical symbolLengths symbols)

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
      let missing = [1..16] \\ lst
          lens = map (\g -> (head g, length g)) $ group lst
       in map (fromIntegral . snd) . sortOn fst $ map (, 0) missing ++ lens

decodeCanonical :: [Word8] -> [Word8] -> HTree Word8
decodeCanonical symLens = mconcat . zipWith codeWordToTree (symbolLengthsToCodes symLens)

instance Semigroup (HTree a) where
  (<>) :: HasCallStack => HTree a -> HTree a -> HTree a
  Nil <> t = t
  t <> Nil = t
  Tree l1 r1 <> Tree l2 r2 = Tree (l1 <> l2) (r1 <> r2)
  _ <> _ = error "overlapping huffman codes"

instance Monoid (HTree a) where
  mempty = Nil

pathMap :: HTree a -> HTree (CodeWord, a)
pathMap = go (CodeWord 0 0)
  where
    go _ Nil = Nil
    go cw (Symbol a) = Symbol (cw, a)
    go (CodeWord cl n) (Tree l r) =
      Tree
        (go (CodeWord (cl + 1) (n `shiftL` 1)) l)
        (go (CodeWord (cl + 1) (n `shiftL` 1 + 1)) r)

codeWordToTree :: CodeWord -> a -> HTree a
codeWordToTree (CodeWord cl n) sym = buildTree cl
  where
    buildTree 0 = Symbol sym
    buildTree h =
      if n `testBit` (h - 1)
        then Tree Nil (buildTree (h - 1))
        else Tree (buildTree (h - 1)) Nil

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
