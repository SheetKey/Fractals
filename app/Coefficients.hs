module Coefficients
  ( Coefficients (..)
  , Point
  , Points
  , rotScale
  , scaleRot
  ) where

type Point = (Double,Double)
type Points = [Point]
type Matrix = (Double,Double,Double,Double)
type Vector = (Double, Double)
data Coefficients = Coefficients Matrix Vector

-- Makes an affine transformation from other transformations
mkAffineCoeffs :: Matrix -> Matrix -> Vector -> Coefficients
mkAffineCoeffs (h,j,k,l) (r,t,y,s) (e,f) = Coefficients (a,b,c,d) (e,f)
  where a = h * r + j * y
        b = h * t + j * s
        c = k * r + l * y
        d = k * t + l * s

mkRotation :: Double -> Matrix
mkRotation theta = (a,b,c,d)
  where a = cos (theta * pi / 180)
        b = - sin (theta * pi / 180)
        c = sin (theta * pi / 180)
        d = cos (theta * pi / 180)
 
mkScale :: Double -> Double -> (Double,Double,Double,Double)
mkScale r s = (a,b,c,d)
  where a = r
        b = 0
        c = 0
        d = s

mkTranslate :: Double -> Double -> (Double,Double)
mkTranslate e f = (e,f)

-- A rotation followed by a scale
rotScale :: Double -> Double -> Double -> Double -> Double -> Coefficients
rotScale theta r s e f = mkAffineCoeffs (mkScale r s) (mkRotation theta) (mkTranslate e f)

-- A sclae followed by a rotation
scaleRot :: Double -> Double -> Double -> Double -> Double -> Coefficients
scaleRot r s theta e f = mkAffineCoeffs (mkScale r s) (mkRotation theta) (mkTranslate e f)
