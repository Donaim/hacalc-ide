
module Initializer where

import Util

import Control.Concurrent as CONC

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

systemRun :: (CompilerState, ReaderState, UIState, CLIState) -> IO ()
systemRun (c, r, ui, cli) = do
	ebin <- eventsBinNew
	CONC.forkIO (reactorLoop ebin c >> return ())
	CONC.forkIO (reactorLoop ebin r >> return ())
	CONC.forkIO (reactorLoop ebin ui >> return ())
	reactorLoop ebin cli >> return ()

newSystemRun :: String -> String -> IO ()
newSystemRun readfile uifile = do
	states <- newSystem readfile uifile
	systemRun states

