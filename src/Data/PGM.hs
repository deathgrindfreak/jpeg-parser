{-# LANGUAGE OverloadedRecordDot #-}

module Data.PGM (jpegToPGM) where

import qualified Data.ByteString.Builder as BSB
import Data.List (foldl1')
import Data.Matrix ((<->), (<|>))
import qualified Data.Matrix as M
import Text.Printf

import Data.Jpeg
import Data.Jpeg.Helper

jpegToPGM :: Jpeg -> BSB.Builder
jpegToPGM jpeg =
  pgmHeader jpeg.headerData.startOfFrame <> pgmBody jpeg
  where
    pgmHeader (StartOfFrame _ h w _) =
      BSB.string7 $ printf "P5\n%d %d\n255\n" w h

    pgmBody jp =
      let m = joinBlocks jp
       in mconcat . map (mconcat . map (BSB.word8 . fromIntegral)) $ M.toLists m

joinBlocks :: Jpeg -> M.Matrix Int
joinBlocks jpeg =
  let blockWidth = jpeg.headerData.startOfFrame.width /// 8
   in foldl1' (<->)
        . map (foldl1' (<|>) . map blockValues)
        . groupByLength blockWidth
        $ concatMap fromBlock jpeg.scanData
  where
    groupByLength _ [] = []
    groupByLength n lst =
      let (l, r) = splitAt n lst
       in l : groupByLength n r
