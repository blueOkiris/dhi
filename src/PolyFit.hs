module PolyFit(polyFit) where

import Data.Matrix(matrix, fromList, inverse, toList, setElem, multStd2, Matrix)
import Debug.Trace

polyFit             :: [(Double, Double)] -> Double -> (Double -> Double)
polyFitStart        :: [(Double, Double)] -> Double -> Int -> (Double -> Double)
averageError        :: [(Double, Double)] -> (Double -> Double) -> Double
genFunction         :: [Double] -> (Double -> Double)
genCoeff            :: [(Double, Double)] -> Int -> [Double]
regressionMatrix    :: ([Double], [Double]) -> Int -> ((Matrix Double), (Matrix Double))
xSqrSum             :: Double -> [Double] -> Double
xSqrMultYSum        :: Double -> [(Double, Double)] -> Double

-- Generate a polynomial function based on best equation
-- This just starts the actual polyfit system (start with degree 0)
polyFit points maxError =
    polyFitStart points maxError 0

-- The actual polyfit
polyFitStart points maxError degreeStart
    | err <= maxError = trace (show err) $
                        function
    | otherwise =       polyFitStart points maxError (degreeStart + 1)
    where
        coeff =         genCoeff points degreeStart
        function =      genFunction coeff
        err =           averageError points function

averageError points function =
    (sum errList) / (fromIntegral $ length errList)
    where
        errFunc =   \(x, y) -> abs $ ((function x) - y) / y
        errList =   map errFunc points

-- Create a function based on our coefficient list
genFunction coeff
    | degree == 0 =
        \x -> curCoeff
    | otherwise =
        \x -> (curCoeff * (x ** (fromIntegral degree))) + (nextFunc x)
    where
        degree =    (length coeff) - 1
        curCoeff =  last coeff
        nextFunc =  genFunction $ init coeff

-- Solve the regression matrices to get a list of coefficients for the equation
genCoeff points degree =
    --trace ((show yMat) ++ "\n" ++ (show invReg) ++ "\n" ++ (show coefMat)) $
    toList coefMat
    where
        (x, y) =            unzip points
        (regMat, yMat) =    regressionMatrix (x, y) degree
        invReg =            either (\str -> fromList 1 1 [0]) (\mat -> mat) $ inverse regMat
        coefMat =           invReg `multStd2` yMat

-- See info on http://polynomialregression.drque.net/math.html
regressionMatrix (x, y) degree =
    --trace ((show y) ++ "\n" ++ (show x) ++ "\n" ++ (show regMat) ++ "\n" ++ (show yMat))
    (fromList (degree + 1) (degree + 1) set11, yMat)
    where
        -- first element is n, all others are the sum of k=0..n of xk ^ (i + j - 2)
        regMat =    matrix (degree + 1) (degree + 1) (\(i, j) -> xSqrSum (fromIntegral $ i + j - 2) x)
        matList =   toList regMat
        set11 =     [ fromIntegral $ length x ] ++ (snd $ splitAt 1 matList)

        -- Column vector, sum k=0..n of xk ^ (j - 1) * yk
        yMat =      matrix (degree + 1) 1 (\(i, j) -> xSqrMultYSum (fromIntegral $ i - 1) (zip x y))

-- The following are used for calculating the regression matrix
-- They've been extracted to full on functions as they mess up the Matrix type (it becomes Int instead of Double) otherwise
xSqrSum expon x =
    sum $ map (\xi -> xi ** expon) x

xSqrMultYSum expon points =
    --trace ("sum of " ++ (show multList) ++ " = " ++ (show $ sum multList)) $
    sum multList 
    where
        multList = map (\(xi, yi) -> (xi ** expon) * yi) points