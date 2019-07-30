
module UserCLI where

import Data.Either
import Control.Monad
import Data.IORef
import Data.Dynamic
import qualified Control.Concurrent as CONC

import PatternT.All

import ICompiler
import IUI
import IReader
import Events

data CLIState = CLIState
	{ stopped     :: Bool
	} deriving (Eq, Show, Read)

instance Reactor CLIState () () where
	reactorStartFlag state = state { stopped = False }
	reactorStoppedQ = stopped
	reactorDelayMS = const 200
	reactorProcess ctx state events = interpretCycle state
	reactorNewCtx ebin state = return ()

userCLINew :: CLIState
userCLINew = CLIState
	{ stopped = True
	}

interpretCycle :: CLIState -> IO (CLIState, [Dynamic])
interpretCycle state = do
	line <- getLine
	let r = interpretLine state line
	return r

interpretLine :: CLIState -> String -> (CLIState, [Dynamic])
interpretLine state line = case line of
	":stop" ->
		(state { stopped = True }, [toDyn $ CompilerStopEvent, toDyn $ ReaderStopEvent, toDyn $ UIStopEvent])

	(_) ->
		(state, [toDyn $ AppendEvaluation line])

