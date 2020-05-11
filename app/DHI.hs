module DHI(integrate) where

import Integral(estimateIntegralEquation)
import PolyFit(polyFit)

-- Use my libraries to generate the integral equation
integrate :: (Double -> Double) -> (Double, Double) -> Double -> Double -> (Double -> Double)
integrate fx bounds stepSize maxError =
    intFx
    where
        dataPoints =    estimateIntegralEquation fx (fst bounds) (snd bounds) stepSize
        intFx =         polyFit dataPoints maxError

-- Export out th polyfit for general use
makePolynomial :: [(Double, Double)] -> Double -> (Double -> Double)
makePolynomial points maxError =
    polyFit points maxError
    