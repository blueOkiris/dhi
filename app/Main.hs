module Main where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Cairo
import System.IO(hSetBuffering, stdout, BufferMode(..))

import DHI

main :: IO ()
main =
    do
        -- Don't buffer inputs (doesn't really matter)
        hSetBuffering stdout NoBuffering

        -- Plot the test equations
        toFile def "test-eq.png" $ do
            layout_title .= "Approximation of Integral Equation"
            plot (line "f(x)" [ testEqCmpr ])
            plot (line "F(x)" [ generatedEq ])
    where
        -- Take the test equation and integrate it
        -- Use bounds -4 to +4, do inner math with step size 0.1, and match polynomial degree with up to 5% error
        testF =             \x -> (3 * x**2) + (2 * x) + 3
        testIntegralFunc =  integrate testF (-4, 4) 0.1 0.05

        -- Generate the points from -4 to +4 with stepSize 0.1, and apply those points to the function
        xList =             take (round $ (4 - (-4)) / 0.1) $ iterate (+ 0.1) (-4)
        generatedEq =       map (\x -> (x, testIntegralFunc x)) xList
        testEqCmpr =        map (\x -> (x, testF x)) xList

