module Main where

import Initializer

main :: IO ()
main = do
	putStrLn "Starting"
	newSystemRun "rules~" "display~"

