
module Initializer where


import Control.Concurrent as CONC
import Control.Concurrent.ParallelIO.Local as CONC
import Text.Read (readMaybe)
import Control.Monad

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

systemRun :: (CompilerState, ReaderState, UIState, CLIState) -> IO String
systemRun (c, r, ui, cli) = do
	ebin <- eventsBinNew
	withEbin ebin
	where
	withEbin ebin = do
		x <- CONC.withPool (length actions) (flip parallel actions)
		return $ show (x !! 0, x !! 1, x !! 2, x !! 3)
		where actions =
			[ (reactorLoop ebin cli >>= return . show)
			, (reactorLoop ebin c >>= return . show)
			, (reactorLoop ebin r >>= return . show)
			, (reactorLoop ebin ui >>= return . show)
			]

newSystemRun :: String -> String -> IO String
newSystemRun readfile uifile = do
	states <- newSystem readfile uifile
	systemRun states

loadSystemRun :: String -> IO String
loadSystemRun path = do
	r <- loadSystem path
	systemRun r

