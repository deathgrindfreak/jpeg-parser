module Data.Color
  ( colorConversion
  , grayscaleConversion
  )
where

clamp :: Double -> Int
clamp = max 0 . min 255 . round

grayscaleConversion :: Double -> Int
grayscaleConversion = clamp . (+ 128)

colorConversion :: Double -> Double -> Double -> (Int, Int, Int)
colorConversion y cr cb =
  let r = cr * (2 - 2 * 0.299) + y
      g = cb * (2 - 2 * 0.114) + y
      b = (y - 0.114 * b - 0.299 * r) / 0.587
   in (clamp (r + 128), clamp (g + 128), clamp (b + 128))
