module Main where
    
import Control.Concurrent
import Data.Foldable
import System.Console.ANSI
import Lib


main = do
    drawBorders
    Just (x, y) <- getTerminalSize
    forM_ [1..(x-2)] (\i -> do
        setCursorPosition i 1
        forM_ [1..(y-2)] (\j ->
            putChar $ case (trace (i - x `div` 2) (j - y `div` 2)) of
                0 -> ' '
                1 -> '░'
                2 -> '▒'
                3 -> '▓'
                4 -> '█'
            )
        )

    updateAll
    threadDelay 2000000
    --clearScreen
