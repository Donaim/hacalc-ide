module Main where

import System.Environment
import Control.Monad

import Initializer

main :: IO ()
main = do
	args <- getArgs

	when (length args /= 2) $ do
		putStrLn $ "Usage: hacalc-ide <rulefile> <displayfile>"

	let rulefile = args !! 0
	let displayfile = args !! 1

	putStrLn "Starting"
	newSystemRun rulefile displayfile

