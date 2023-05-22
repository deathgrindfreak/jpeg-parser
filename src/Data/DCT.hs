module Data.DCT (idct)
where

import qualified Data.Vector as V
import qualified Data.Matrix as M

zigZag :: M.Matrix Int
zigZag =
  M.fromLists
    [ [0, 1, 5, 6, 14, 15, 27, 28]
    , [2, 4, 7, 13, 16, 26, 29, 42]
    , [3, 8, 12, 17, 25, 30, 41, 43]
    , [9, 11, 18, 24, 31, 40, 44, 53]
    , [10, 19, 23, 32, 39, 45, 52, 54]
    , [20, 22, 33, 38, 46, 51, 55, 60]
    , [21, 34, 37, 47, 50, 56, 59, 61]
    , [35, 36, 48, 49, 57, 58, 62, 63]
    ]

idctPrecision :: Int
idctPrecision = 8

idctTable :: M.Matrix Double
idctTable =
  M.fromLists
    [ [ let u' = fromIntegral u
            x' = fromIntegral x
         in normCoeff u' * cos (((2.0 * x' + 1.0) * u' * pi) / 16.0)
      | x <- [0 .. idctPrecision - 1]
      ]
    | u <- [0 .. idctPrecision - 1]
    ]
  where
    normCoeff n = if n == 0 then 1.0 / sqrt 2.0 else 1.0

applyZigZag :: V.Vector Int -> M.Matrix Int
applyZigZag bc = M.matrix 8 8 $ \(i, j) -> bc V.! (zigZag M.! (i, j))

idct :: V.Vector Int -> M.Matrix Int
idct bc =
  let out = fromIntegral <$> applyZigZag bc
   in M.matrix 8 8 $ \(y, x) ->
        let s =
              [ (out M.! (v, u)) * (idctTable M.! (u, x)) * (idctTable M.! (v, y))
              | u <- [1 .. idctPrecision]
              , v <- [1 .. idctPrecision]
              ]
         in round $ sum s / 4
