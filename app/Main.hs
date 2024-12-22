module Main where
import Game (testconnections, connections)
import System.Console.Haskeline (runInputT, defaultSettings)

main :: IO ()
main = connections

