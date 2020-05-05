module Main where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Cairo
import System.IO(hSetBuffering, stdout, BufferMode(..))

import Integral(estimateDefiniteIntegral, estimateIntegralEquation)
import PolyFit(polyFit)

main :: IO ()
main =
    do
        hSetBuffering stdout NoBuffering
        --putStrLn $ show testIntegral1
        toFile def "test-eq.png" $ do
            layout_title .= "Approximation of Integral Equation"
            setShapes   [ PointShapeCircle
                        , PointShapeCircle ]
            plot (line      "f(x)"  [ testEqCmpr ])
            plot (points    "F(x)"  testIntegralEq)
            plot (line      "~F(x)" [ generatedEq ])
    where
        --testIntegral1 =     estimateDefiniteIntegral (\x -> 2 * x) 0 10 0.000001

        testF =             \x -> (3 * x**2) + (2 * x) + 3
        testIntegralEq =    estimateIntegralEquation testF (-4) 4 0.1
        testEqCmpr =        map (\(x, y) -> (x, testF x)) testIntegralEq

        testIntegralFunc =  polyFit testIntegralEq 0.05      -- 5 % error
        generatedEq =       map (\(x, y) -> (x, testIntegralFunc x)) testIntegralEq

