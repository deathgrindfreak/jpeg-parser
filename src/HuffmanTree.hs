{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module HuffmanTree
  ( HuffmanTree (..)
  , huffmanTree
  , showTree
  , mkTree
  )
where

import Data.Attoparsec.ByteString.Lazy
import Data.Bits (shiftL, testBit)
import Data.List (foldl', intercalate)
import Data.Word (Word8)
import GHC.Stack (HasCallStack)

import Helper.Parser

data CodeWord = CodeWord Int Word8
  deriving (Show)

data HuffmanTree = HuffmanTree
  { info :: Word8
  , tree :: Tree Word8
  }
  deriving (Show)

data Tree a = Nil | Leaf a | Tree (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

empty :: Tree a
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

mkTree :: [Word8] -> [Word8] -> Tree Word8
mkTree syms = mconcat . zipWith codeWordToTree syms . symbolLengthsToCodes

instance Semigroup (Tree a) where
  (<>) :: HasCallStack => Tree a -> Tree a -> Tree a
  Nil <> Nil = Nil
  Nil <> t = t
  t <> Nil = t
  Leaf _ <> _ = error "overlapping huffman codes"
  _ <> Leaf _ = error "overlapping huffman codes"
  Tree l1 r1 <> Tree l2 r2 =
    Tree (mappend l1 l2) (mappend r1 r2)

instance Monoid (Tree a) where
  mempty = Nil

codeWordToTree :: a -> CodeWord -> Tree a
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

height :: Tree a -> Int
height t = go 0 t
  where
    go h (Tree l r) = max (go (h + 1) l) (go (h + 1) r)
    go h _ = h

data FillTree a = End a | FillTree a (FillTree a) (FillTree a)
  deriving (Show)

showTree :: Tree a -> String
showTree t =
  unlines
    . map (\(n, s) -> (lpad n ++) . intercalate (spacing n) . map toSym $ s)
    . (zip [1 ..])
    . treeToList
    . fillTree
    $ t
  where
    h = height t

    lpad n = replicate (2 ^ (h - n + 1) - 1) ' '

    spacing 1 = ""
    spacing n = replicate (2 ^ (h - n + 2) - 1) ' '

    toSym Nothing = "*"
    toSym (Just _) = "+"

fillTree :: HasCallStack => Tree a -> FillTree (Maybe a)
fillTree t = go 0 t
  where
    h = height t

    go currH currTree
      | h == currH =
          case currTree of
            Nil -> End Nothing
            Leaf a -> End (Just a)
            _ -> error "exceeded tree height"
      | otherwise =
          let h' = currH + 1
           in case currTree of
                Nil -> FillTree Nothing (go h' Nil) (go h' Nil)
                Leaf a -> FillTree (Just a) (go h' Nil) (go h' Nil)
                Tree l r ->
                  FillTree Nothing (go h' l) (go h' r)

treeToList :: FillTree a -> [[a]]
treeToList (End a) = [[a]]
treeToList (FillTree a l r) =
  [a] : zipWith (++) (treeToList l) (treeToList r)
