module Data.HuffmanTree.Helper (showTree) where

import Data.List (intercalate)
import GHC.Stack (HasCallStack)

import Data.HuffmanTree.Model

data FillTree a = End a | FillTree a (FillTree a) (FillTree a)
  deriving (Show)

showTree :: HTree a -> String
showTree t =
  unlines
    . zipWith mkRow [1 ..]
    . treeToList
    . fillTree
    $ t
  where
    h = height t

    mkRow n = (lpad n ++) . intercalate (spacing n) . map toSym

    lpad n = replicate (2 ^ (h - n + 1) - 1) ' '

    spacing 1 = ""
    spacing n = replicate (2 ^ (h - n + 2) - 1) ' '

    toSym Nothing = "*"
    toSym (Just _) = "+"

fillTree :: HasCallStack => HTree a -> FillTree (Maybe a)
fillTree t = go 0 t
  where
    h = height t

    go currH currTree
      | h == currH =
          case currTree of
            Nil -> End Nothing
            Symbol a -> End (Just a)
            _ -> error "exceeded tree height"
      | otherwise =
          let h' = currH + 1
           in case currTree of
                Nil -> FillTree Nothing (go h' Nil) (go h' Nil)
                Symbol a -> FillTree (Just a) (go h' Nil) (go h' Nil)
                Tree l r ->
                  FillTree Nothing (go h' l) (go h' r)

treeToList :: FillTree a -> [[a]]
treeToList (End a) = [[a]]
treeToList (FillTree a l r) =
  [a] : zipWith (++) (treeToList l) (treeToList r)

height :: HTree a -> Int
height = go 0
  where
    go h (Tree l r) = max (go (h + 1) l) (go (h + 1) r)
    go h _ = h
