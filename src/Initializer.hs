
module Initializer where


import Control.Concurrent as CONC
import Control.Concurrent.ParallelIO.Local as CONC
import Text.Read (readMaybe)
import Control.Monad
import Data.Dynamic

import Util
import Events
import SimpleReader
import SimpleUI
import Compiler
import UserCLI

newSystem :: String -> String -> IO (CompilerState, ReaderState, UIState, CLIState)
newSystem readfile uifile = do
	let c = compilerNew
	r <- simpleReaderNew readfile
	let ui = simpleUINew uifile
	let cli = userCLINew
	return (c, r, ui, cli)

loadSystem :: String -> IO (CompilerState, ReaderState, UIState, CLIState)
loadSystem path = do
	text <- readFile path
	case readMaybe text of
		Nothing -> fail $ "Could not parse text:\n" ++ show text
		Just pairs -> return pairs

	where
	parsearr parts = do
		guard (length parts == 4)
		c <- readMaybe (parts !! 0)
		r <- readMaybe (parts !! 1)
		ui <- readMaybe (parts !! 2)
		cli <- readMaybe (parts !! 3)
		return (c, r, ui, cli)

systemRun :: (CompilerState, ReaderState, UIState, CLIState) -> IO (CompilerState, ReaderState, UIState, CLIState)
systemRun (c, r, ui, cli) = do
	ebin <- eventsBinNew
	withEbin ebin
	where
	castn x n = fromDyn (x !! n) (error $ "COULD NOT CAST " ++ show x)
	withEbin ebin = do
		x <- CONC.withPool (length actions) (flip parallel actions)
		return (castn x 0, castn x 1, castn x 2, castn x 3)
		where actions =
			[ (reactorLoop ebin c >>= return . toDyn)
			, (reactorLoop ebin r >>= return . toDyn)
			, (reactorLoop ebin ui >>= return . toDyn)
			, (reactorLoop ebin cli >>= return . toDyn)
			]

newSystemRun :: String -> String -> IO String
newSystemRun readfile uifile = do
	states <- newSystem readfile uifile
	r <- systemRun states
	return $ show r

loadSystemRun :: String -> IO String
loadSystemRun path = do
	l <- loadSystem path
	r <- systemRun l
	return $ show r

