module Solver
  ( fromCoeffs
  ) where

import Coefficients

type Row = [Double]
type Matrix = [Row]

------------------------------------------------------------------------------------

-- Convert coefficients into a matrix.
fromInit :: Coefficients -> Matrix
fromInit (Coefficients (a,b,c,d) (e,f)) = [[a-1,b,-e],[c,d-1,-f]]

-- Convert solution Row into an initial value pair.
fromRow :: Row -> Points
fromRow r
  | length r == 2 = [(head r, r !! 1)]
  | otherwise     = []

-- Turn coefficients into a matrix and solve for the points.
fromCoeffs :: [Coefficients] -> Points
fromCoeffs = foldr (\p -> (++) (fromRow $ solve $ fromInit p)) []

------------------------------------------------------------------------------------

  
gaussianReduce :: Matrix -> Matrix
gaussianReduce matrix = fixlastrow $ foldl reduceRow matrix [0..length matrix-1] where
 
 --swaps element at position a with element at position b.
 swap xs a b
  | a > b = swap xs b a
  | a == b = xs
  | a < b =
    let
      (p1,p2) = splitAt a xs
      (p3,p4) = splitAt (b-a-1) (tail p2)
    in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ tail p4
 
 reduceRow matrix1 r = let
  --first non-zero element on or below (r,r).
  firstnonzero = head $ filter (\x -> matrix1 !! x !! r /= 0) [r..length matrix1-1]
 
  --matrix with row swapped (if needed)
  matrix2 = swap matrix1 r firstnonzero
 
  --row we're working with
  row = matrix2 !! r
 
  --make it have 1 as the leading coefficient
  row1 = map (\x -> x / (row !! r)) row
 
  --subtract nr from row1 while multiplying
  subrow nr = let k = nr!!r in zipWith (\a b -> k*a - b) row1 nr
 
  --apply subrow to all rows below
  nextrows = map subrow $ drop (r+1) matrix2
 
  --concat the lists and repeat
  in take r matrix2 ++ [row1] ++ nextrows
 
 fixlastrow matrix' = let
  a = init matrix'; row = last matrix'; z = last row; nz = last (init row)
  in a ++ [init (init row) ++ [1, z / nz]]

 --Solve a matrix (must already be in REF form) by back substitution.
substitute :: Matrix -> Row
substitute matrix = foldr next [last (last matrix)] (init matrix) where
 
 next row found = let
  subpart = init $ drop (length matrix - length found) row
  solution = last row - sum (zipWith (*) found subpart)
  in solution : found

solve :: Matrix -> Row
solve = substitute . gaussianReduce
