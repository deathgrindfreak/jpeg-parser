module Data.Jpeg.PrintInfo where

import qualified Data.Vector as V
import Data.List (groupBy)
import Data.Function (on)
import Text.Printf

import Data.Jpeg.Model
import Data.HuffmanTree
import Data.CodeWord

import Debug.Trace

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
printHuffmanTrees hts = unlines (map printHuffmanTree hts)

printHuffmanTree :: HuffmanTree -> String
printHuffmanTree (HuffmanTree qt tt t) =
  let groups =
        map (\i -> let l = codeWordLength . fst $ head i
                    in (l, map snd i))
          . groupBy (on (==) (codeWordLength . fst))
          . flattenTree $ t

      header :: Int -> Int -> String
      header i t = printf "Codes of length %02d bits (%03d total):" i t
   in unlines $ map
      (\i ->
         case lookup i groups of
           Nothing -> header i 0
           Just ns -> header i (sum . map fromIntegral $ ns) ++ unwords (map show ns)
         )
      [0..16]
