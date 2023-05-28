module Data.Color
  ( colorConversion
  , colorConversionMatrix
  , grayscaleConversion
  , Color (..)
  , toList
  )
where

import Data.Matrix (Matrix)
import qualified Data.Matrix as M

clamp :: Double -> Int
clamp = max 0 . min 255 . round

data Color
  = GrayScale Int
  | RGB Int Int Int
  deriving (Eq, Show)

toList :: Color -> [Int]
toList (GrayScale g) = [g]
toList (RGB r g b) = [r, g, b]

grayscaleConversion :: Int -> Color
grayscaleConversion = GrayScale . clamp . (+ 128) . fromIntegral

colorConversion :: Int -> Int -> Int -> Color
colorConversion y cb cr =
  let (y', cb', cr') = (fromIntegral y, fromIntegral cb, fromIntegral cr)
      r = cr' * (2 - 2 * 0.299) + y'
      b = cb' * (2 - 2 * 0.114) + y'
      g = (y' - 0.114 * b - 0.299 * r) / 0.587
   in RGB (clamp (r + 128)) (clamp (g + 128)) (clamp (b + 128))

colorConversionMatrix ::
  Matrix Int ->
  Matrix Int ->
  Matrix Int ->
  Matrix Color
colorConversionMatrix m1 m2 m3 =
  M.matrix (M.nrows m1) (M.ncols m1) $
    \(i, j) ->
      colorConversion
        (M.unsafeGet i j m1)
        (M.unsafeGet i j m2)
        (M.unsafeGet i j m3)
