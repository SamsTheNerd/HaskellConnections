module Main where
import Game (testconnections)
import System.Console.Haskeline (runInputT, defaultSettings)

main :: IO ()
main = testconnections

