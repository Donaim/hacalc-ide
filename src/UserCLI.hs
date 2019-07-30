
module UserCLI where

import Data.Either
import Control.Monad
import Data.IORef
import Data.Dynamic
import qualified Control.Concurrent as CONC

import PatternT.All

import ICompiler
import IUI
import Events

data CLIState = CLIState
	{ stopped     :: Bool
	} deriving (Eq, Show, Read)

instance Reactor CLIState () () where
	reactorStoppedQ = stopped
	reactorDelayMS = const 0
	reactorProcess ctx state events = return (state, [])
	reactorNewCtx ebin state = return ()

userCLINew :: CLIState
userCLINew = CLIState
	{ stopped = False
	}

interpretCycle :: CLIState -> IO (CLIState, [Dynamic])
interpretCycle state = do
	line <- getLine
	return $ interpretLine state line

interpretLine :: CLIState -> String -> (CLIState, [Dynamic])
interpretLine state line = case line of
	":stop" ->
		(state { stopped = True }, [])

	(_) ->
		(state, [toDyn $ AppendEvaluation line])

