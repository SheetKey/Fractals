module Graph
  ( myGetLine
  , mkPlotIO
  ) where

import Graphics.Rendering.Chart.Easy
    ( filledCircles,
      layout_plots,
      layout_title,
      plot_points_style,
      plot_points_title,
      plot_points_values,
      opaque,
      red,
      (.~),
      ToPlot(toPlot),
      PickFn,
      Renderable,
      ToRenderable(toRenderable),
      Default(def) )
import Graphics.Rendering.Chart.Backend.Cairo ( renderableToFile )

import Data.Colour ()
import Data.Colour.Names ()
import Data.Default.Class ()
import Control.Lens ()

import Solver ()
import Fractal ( generateFractal )
import Coefficients ( Coefficients, Points, rotScale, scaleRot )

import System.IO (hFlush, stdout, IOMode (AppendMode))
import System.Exit (exitSuccess)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import Data.List.Extra (splitOn)
import Control.Monad.Trans.State (StateT, modify, get, evalStateT)
import Control.Monad.IO.Class (MonadIO(liftIO))

-- a helper for savePlot
makeGraph :: Points -> String -> Renderable ()
makeGraph points title = toRenderable layout
  where
    myPlot = plot_points_style .~ filledCircles 0.5 (opaque red)
           $ plot_points_values .~ points
           $ plot_points_title .~ ""
           $ def

    layout = layout_title .~ title
           $ layout_plots .~ [toPlot myPlot]
           $ def

-- Plots the fractal
savePlot :: Points -> String -> String -> IO (PickFn ())
savePlot points name title = renderableToFile def ("./" ++ name) (makeGraph points title)

myGetLine :: IO String
myGetLine = do
  str <- getLine
  if str == ":q" then exitSuccess else return str

-- Validates an integer            
validateInt :: String -> Maybe Int
validateInt = readMaybe

-- Gets coeffs for a rotScale
rotScaleIO :: IO Coefficients
rotScaleIO = do
  putStrLn "Enter a comma separated list of values: "
  putStrLn "Note: x and y scaling values must be between 0 and 1."
  putStrLn "Angle for rotation (degrees), x scaling, y scaling, x offset, y offset"
  list <- myGetLine
  let nospace = filter (/= ' ') list
      splitList = splitOn "," nospace
      maybeDub :: [Maybe Double]
      maybeDub = map readMaybe splitList
  if Nothing `elem` maybeDub
    then do
    putStrLn "Invalid input"
    rotScaleIO
    else if length splitList == 5
            && 0.0 <= read (splitList !! 1)
            && 1.0 >= read (splitList !! 1)
            && 0.0 <= read (splitList !! 2)
            && 1.0 >= read (splitList !! 2)
         then return $ rotScale (read $ head splitList)
                                (read $ splitList !! 1)
                                (read $ splitList !! 2)
                                (read $ splitList !! 3)
                                (read $ splitList !! 4)
         else do
         putStrLn "Invalid input"
         rotScaleIO

-- Gets coeffs for a scaleRot
scaleRotIO :: IO Coefficients
scaleRotIO = do
  putStrLn "Enter a comma separated list of values: "
  putStrLn "Note: x and y scaling values must be between 0 and 1."
  putStrLn "x scaling, y scaling, angle for rotation (degrees), x offset, y offset"
  list <- myGetLine
  let nospace = filter (/= ' ') list
      splitList = splitOn "," nospace
      maybeDub :: [Maybe Double]
      maybeDub = map readMaybe splitList
  if Nothing `elem` maybeDub
    then do
    putStrLn "Invalid input"
    scaleRotIO
    else if length splitList == 5
            && 0.0 <= read (head splitList)
            && 1.0 >= read (head splitList)
            && 0.0 <= read (splitList !! 1)
            && 1.0 >= read (splitList !! 1)
         then return $ scaleRot (read $ head splitList)
                                (read $ splitList !! 1)
                                (read $ splitList !! 2)
                                (read $ splitList !! 3)
                                (read $ splitList !! 4)
         else do
         putStrLn "Invalid input"
         scaleRotIO

-- Gets coeffs
mkCoeff :: IO Coefficients
mkCoeff = do
  putStrLn "Rotation followed by scale (enter '1') or scale folloed by rotation (enter '2')"
  n <- myGetLine
  case validateInt n of
    Nothing -> do
      putStrLn "Invalid input"
      mkCoeff
    Just a | a == 1 -> rotScaleIO
           | a == 2 -> scaleRotIO
           | otherwise -> do
               putStrLn "Invalid input"
               mkCoeff

mkCoeffs :: StateT Int IO [Coefficients]
mkCoeffs = do
  coeffs <- liftIO mkCoeff
  n <- get
  if n > 1
    then do
      modify (flip (-) 1)
      lst <- mkCoeffs
      return $ coeffs : lst
    else return [coeffs]


-- Gets coefficiets from the userError
getCoeffs :: IO [Coefficients]
getCoeffs = do
  putStr "Number of equations: "
  hFlush stdout
  n <- myGetLine
  case validateInt n of
    Nothing -> do
      putStrLn "Invalid Input"
      getCoeffs
    Just a -> if a <=1
              then do
                putStrLn "Invalid input."
                getCoeffs
              else evalStateT mkCoeffs a


howManyPoints :: IO Int
howManyPoints = do
  putStrLn "How many points should be plotted? (At least 10000 for something interesting)"
  str <- myGetLine
  case validateInt str of
    Nothing -> do
      putStrLn "Invalid input"
      howManyPoints
    Just a -> if a > 0
              then return a
              else do
                putStrLn "Invalid input"
                howManyPoints

-- Gets input to generate the fractal
generateFractalIO :: IO Points
generateFractalIO = do
  coeffs <- getCoeffs
  generateFractal coeffs <$> howManyPoints

getFileName :: IO String
getFileName = do
  putStrLn "Save file as"
  name <- myGetLine
  case name of
    "" -> do
      putStrLn "Invalid input"
      getFileName
    _  -> return name

getTitle :: IO String
getTitle = do
  putStrLn "Image title. Leave blank for no title."
  myGetLine

mkPlotIO :: IO ()
mkPlotIO = do
  points <- generateFractalIO
  filename <- getFileName
  title <- getTitle
  savePlot points filename title
  return ()
