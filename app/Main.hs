{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment
import Control.Monad
import System.Console.CmdArgs
import Data.Data

import Initializer

data ArgsT
	= New
	{ rulefile :: String
	, displayfile :: String
	, sessionfile :: Maybe String
	}
	| Load
	{ loadfile :: String
	}
	deriving (Show, Data, Typeable)

new = New
	{ rulefile = def &= argPos 0 &= typ "rulefile"
	, displayfile = def &= argPos 1 &= typ "displayfile"
	, sessionfile = def &= typ "path"
	}
load = Load
	{ loadfile = def &= argPos 2 &= typ "loadfile"
	}

main :: IO ()
main = do
	args <- cmdArgs $ modes [new, load]
		&= help "Console based ide for PatternT"
		&= program "hacalc-ide"

	putStrLn "Starting"

	case args of
		(New {}) -> do
			showed <- newSystemRun (rulefile args) (displayfile args)

			case sessionfile args of
				Nothing -> return ()
				Just file -> do
					writeFile file showed
					putStrLn $ "Session saved to " ++ file

		(Load {}) -> do
			showed <- loadSystemRun (loadfile args)
			writeFile (loadfile args) showed
			putStrLn $ "Session saved to " ++ (loadfile args)


