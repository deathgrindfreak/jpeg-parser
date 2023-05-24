{-# LANGUAGE OverloadedRecordDot #-}

module Data.PGM (jpegToPGM) where

import qualified Data.Matrix as M
import Data.List (intercalate)
import Text.Printf

import Data.Jpeg

jpegToPGM :: Jpeg -> String
jpegToPGM jpeg =
  pgmHeader jpeg.headerData.startOfFrame ++ "\n" ++ pgmBody jpeg.scanData
  where
    pgmHeader (StartOfFrame _ h w _) = printf "P5 \n%d %d\n255" w h

    pgmBody =
      unlines . concatMap (map (intercalate "\n" . map (unwords . map show) . M.toLists . blockValues) . fromBlock)
