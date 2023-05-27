module Data.Color
  ( colorConversion
  , grayscaleConversion
  , Color (..)
  , toList
  )
where

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
      g = cb' * (2 - 2 * 0.114) + y'
      b = (y' - 0.114 * b - 0.299 * r) / 0.587
   in RGB (clamp (r + 128)) (clamp (g + 128)) (clamp (b + 128))
