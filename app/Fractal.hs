module Fractal
  ( generateFractal
  ) where

import Solver
import Coefficients
import Data.List.Extra (nubOrd)

-- Transfomr a point using coefficients.
coeffMultiply :: Coefficients -> Point -> Point
coeffMultiply (Coefficients (a,b,c,d) (e,f)) (x,y) = (a * x + b * y + e, c * x + d * y + f)

-- Transform a point using an IFS.
pointIFS :: [Coefficients] -> Point -> Points
pointIFS [] _ = []
pointIFS (c:cs) pt = coeffMultiply c pt : pointIFS cs pt

-- Transform points using an IFS.
pointsIFS :: [Coefficients] -> Points -> Points
pointsIFS cs pts = pts >>= pointIFS cs

-- Run the IFS until there are n points
largeIFS :: [Coefficients] -> Points -> Int -> Points
largeIFS cs pts n | length pts < n          = largeIFS cs (pointsIFS cs pts) n
                  | length (nubOrd pts) < n = pointsIFS cs pts
                  | otherwise               = pts

-- Generate the points of a fractal from coefficients and a specified number of points.
generateFractal :: [Coefficients] -> Int -> Points
generateFractal cs = largeIFS cs (fromCoeffs cs) 
