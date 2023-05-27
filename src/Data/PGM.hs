{-# LANGUAGE OverloadedRecordDot #-}

module Data.PGM (jpegToPGM) where

import qualified Data.ByteString.Builder as BSB
import Data.List (foldl1')
import Data.Matrix ((<->), (<|>))
import qualified Data.Matrix as M
import Text.Printf

import Data.Color
import Data.Jpeg
import Data.Jpeg.Helper

jpegToPGM :: Jpeg -> BSB.Builder
jpegToPGM jpeg =
  pgmHeader jpeg.headerData.startOfFrame <> pgmBody jpeg
  where
    pgmHeader (StartOfFrame _ h w _) =
      BSB.string7 $ printf "P5\n%d %d\n255\n" w h

    pgmBody =
      mconcat
        . concatMap (map (BSB.word8 . fromIntegral) . toList)
        . M.toList
        . joinBlocks

joinBlocks :: Jpeg -> M.Matrix Color
joinBlocks jpeg =
  let blockWidth = jpeg.headerData.startOfFrame.width /// 8
   in foldl1' (<->)
        . map (foldl1' (<|>))
        . groupByLength blockWidth
        $ jpeg.scanData
  where
    groupByLength _ [] = []
    groupByLength n lst =
      let (l, r) = splitAt n lst
       in l : groupByLength n r
