module Data.Jpeg.PrintInfo (printJpeg) where

import qualified Data.Vector as V
import Data.List (groupBy)
import Data.Function (on)
import Text.Printf

import Data.Jpeg.Model
import Data.HuffmanTree
import Data.CodeWord

printJpeg :: JpegData -> String
printJpeg (JpegData q sof h) =
  unlines
    [ "Quantization Tables:\n" ++ unlines (zipWith printQuantization q [0 ..])
    , printStartOfFrame sof
    , printHuffmanTrees h
    ]

printQuantization :: QuantizationTable -> Int -> String
printQuantization (QuantizationTable t table) idNum =
  unlines
    [ "Destination ID=" ++ show idNum
    , unlines
        . zipWith
          ( \rowNum row ->
              "  DQT, Row #" ++ show rowNum ++ ": " ++ unwords row
          )
          [0 ..]
        . groupByLength 8
        . map show
        . V.toList
        $ table
    ]
  where
    groupByLength _ [] = []
    groupByLength n lst =
      let (l, r) = splitAt n lst
       in l : groupByLength n r

printStartOfFrame :: StartOfFrame -> String
printStartOfFrame _ = ""

printHuffmanTrees :: [HuffmanTree] -> String
printHuffmanTrees = unlines . map printHuffmanTree

printHuffmanTree :: HuffmanTree -> String
printHuffmanTree (HuffmanTree qt tt t) =
  let nodeList = flattenTree t

      groups =
        map (\i -> let l = codeWordLength . fst $ head i
                    in (l, map snd i))
          . groupBy (on (==) (codeWordLength . fst))
          $ nodeList

      header :: Int -> Int -> String
      header = printf "  Codes of length %02d bits (%03d total): "

      codeRows = unlines $ map
                   (\i ->
                      case lookup i groups of
                        Nothing -> header i 0
                        Just ns -> header i (length ns) ++ unwords (map (printf "%02x") ns)
                      )
                   [0..16]
   in unlines
        [ "Type (" ++ show tt ++ ", " ++ show qt ++ ")"
        , codeRows
        , printf "  Total number of codes: %03d" (length nodeList)
        ]
