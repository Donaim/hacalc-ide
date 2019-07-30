
module Initializer where

import Util

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
	undefined


