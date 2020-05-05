module Integral(estimateDefiniteIntegral, estimateIntegralEquation) where

--import Numeric.Limits(minValue)
--import Debug.Trace

-- Function definitions
estimateDefiniteIntegral    :: (Double -> Double) -> Double -> Double -> Double -> Double
estimateIntegralEquation    :: (Double -> Double) -> Double -> Double -> Double -> [(Double, Double)]
integrateStep               :: (Double -> Double) -> Double -> Double -> Double
getIntegralPoint            :: (Double -> Double) -> Double -> Double -> Double

-- Constants
{-stepSize :: Double
stepSize = minValue :: Double-}

{-
 - Use the integral points to generate
 - x and y points for a general "integral equation"
 -
 - It turns out, F(x) = {
 -                        F(x) - F(0) = int(f(t), t=0..x), x >= 0
 -                        -(F(0) - F(x)) = -int(f(t), t=x..0), x < 0
 -                      }
 -}
estimateIntegralEquation f min max stepSize =
    --trace (show range) $
    map (\x -> (x, getIntegralPoint f x stepSize)) range
    where
        numSteps = round $ (max - min) / stepSize
        range = map (+ min) $ map (* stepSize) (take numSteps [0..])
        

getIntegralPoint f x stepSize
    | x >= 0 =
        estimateDefiniteIntegral f 0 x (stepSize / 4)
    | x <= 0 =
        -1 * (estimateDefiniteIntegral f x 0 (stepSize / 4))

{-
 - Exported apporximation of integral using Riemann sums
 - 
 - Essentially, start from lower bound
 - get the value just before and just after (smallest ste possible)
 - then multiply by step size
 - Then add to the value of the recurred method,
 - this time starting at lowerBound + smallest step size
 - All the way until you reach 
 -
 - ^
 - |       __________
 - |  _____| | || | |
 - |  | | || | || | |_____
 - |  | | || | || | || | |
 - --------------------------->
 - The center poles are the lowerBounds
 - Each is a rectangle of width 2 step sizes
 - and each is 2 step sizes appart
 -}
estimateDefiniteIntegral f lowerBound upperBound stepSize
    | lowerBound >= upperBound =
        --trace (show currentRect)
        currentRect
    | otherwise =
        --trace ((show currentRect) ++ " + " ++ (show sumOfNextRects)) $
        currentRect + sumOfNextRects
    where
        currentRect =       --trace ("Lower Bound: " ++ (show lowerBound) ++ "\nUpper Bound: " ++ (show upperBound) ++ "\nStep: " ++ (show stepSize)) $
                            integrateStep f lowerBound stepSize
        sumOfNextRects =    estimateDefiniteIntegral f (lowerBound + (2 * stepSize)) upperBound stepSize

-- This takes a center point, and returns the area of the rect around it
integrateStep f center stepSize =
    {-
     - _____
     - | | |
     - | | |
     - -----
     -
     - (step + step) * f(center)
     -}
    2 * stepSize * (f center)
