{-# LANGUAGE TypeApplications #-}

module Data.Jpeg.PrintInfo (printJpeg) where

import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Matrix as M
import Text.Printf

import Data.CodeWord
import Data.DCT
import Data.HuffmanTree
import Data.Jpeg.Model

printJpeg :: JpegData -> String
printJpeg (JpegData q sof h) =
  "==JPEG Header Info=="
    ++ "\n\n"
    ++ "Quantization Tables:\n"
    ++ unlines (zipWith printQuantization q [0 ..])
    ++ printStartOfFrame sof
    ++ "\n"
    ++ printHuffmanTrees h
    ++ "\n"

printQuantization :: QuantizationTable -> Int -> String
printQuantization (QuantizationTable t table) idNum =
  "  Destination ID=" ++ show idNum ++ " (" ++ show t ++ ")\n" ++ tableText table
  where
    tableText =
      unlines
        . zipWith @Int
          ( \rowNum row ->
              "    DQT, Row #" ++ show rowNum ++ ": " ++ unwords row
          )
          [0 ..]
        . groupByLength 8
        . map show
        . M.toList
        . applyZigZag

    groupByLength _ [] = []
    groupByLength n lst =
      let (l, r) = splitAt n lst
       in l : groupByLength n r

printStartOfFrame :: StartOfFrame -> String
printStartOfFrame (StartOfFrame p h w cs) =
  "Start of Frame\n"
    ++ "  Precision: "
    ++ show p
    ++ "\n"
    ++ "  Height: "
    ++ show h
    ++ "\n"
    ++ "  Width: "
    ++ show w
    ++ "\n"
    ++ "  Components:\n"
    ++ unlines (map showComponents cs)
  where
    showComponents (Component ct sf qtNum) =
      "    Component: "
        ++ show ct
        ++ "\n"
        ++ "    Sampling Factor: "
        ++ show sf
        ++ "\n"
        ++ "    Quantization Table Number: "
        ++ show qtNum

printHuffmanTrees :: [HuffmanTree] -> String
printHuffmanTrees = unlines . map printHuffmanTree

printHuffmanTree :: HuffmanTree -> String
printHuffmanTree (HuffmanTree qt tt t) =
  let nodeList = flattenTree t

      groups =
        map
          ( \i ->
              let l = codeWordLength . fst $ head i
               in (l, map snd i)
          )
          . groupBy (on (==) (codeWordLength . fst))
          $ nodeList

      header :: Int -> Int -> String
      header = printf "  Codes of length %02d bits (%03d total): "

      codeRows =
        unlines $
          map
            ( \i ->
                case lookup i groups of
                  Nothing -> header i 0
                  Just ns -> header i (length ns) ++ unwords (map (printf "%02x") ns)
            )
            [0 .. 16]
   in "Type ("
        ++ show tt
        ++ ", "
        ++ show qt
        ++ ")"
        ++ codeRows
        ++ printf "  Total number of codes: %03d" (length nodeList)
        ++ "\n"
