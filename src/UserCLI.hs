
module UserCLI where

import Text.Read (readMaybe)
import Data.Either
import Data.Maybe
import Data.Char
import Control.Exception
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
	mline <- try getLine
	case mline :: Either SomeException String of
		Left e -> do
			putStrLn $ "ERROR READING LINE: " ++ show e
			return (state, [])
		Right line -> do
			let r = interpretLine state line
			return r

interpretLine :: CLIState -> String -> (CLIState, [Dynamic])
interpretLine state line = case cmdParse line of
	Stop ->
		(state { stopped = True }, [toDyn $ CompilerStopEvent, toDyn $ ReaderStopEvent, toDyn $ UIStopEvent])
	Eval line ->
		(state, [toDyn $ AppendEvaluation line])

	Error s -> do
		(state, [toDyn $ Notify $ "Error: " ++ s])
	ErrorNoCmd s ->
		(state, [toDyn $ Notify $ "Command " ++ show s ++ " does not exists"])

	Remove i ->
		(state, [toDyn $ UIRemoveEvaluation i, toDyn $ CompilerRemoveEvalRecord i])
	Reset ->
		(state, [toDyn $ ClearEvaluations, toDyn $ CompilerResetEvaluations])
	Limit n ->
		(state, [toDyn $ UIChangeLimit n, toDyn $ CompilerChangeLimit n])
	Whitespace ->
		(state, [])

data Cmd
	= Error String
	| ErrorNoCmd String
	| Eval String
	| Stop
	| Reset
	| Remove Int
	| Limit Int
	| Whitespace
	deriving (Eq, Show, Read)

eguard :: Bool -> a -> b -> Either a b
eguard bad a b = if bad then Left a else Right b

uneither :: Either x x -> x
uneither x = case x of
	Left a -> a
	Right b -> b

cmdParse :: String -> Cmd
cmdParse line = do
	if all isSpace line
	then Whitespace
	else if isEval
		then Eval line
		else case prefix of
			"quit" ->
				Stop
			"reset" ->
				Reset
			"rm" -> uneither $ do
				x <- eguard (null args)
					(Error $ "rm: expected single argument but got " ++ show (length args))
					(head args)

				let mayben = readMaybe x :: Maybe Int
				n <- eguard (isNothing mayben)
					(Error $ ":rm expected nonegative Int but got " ++ x)
					(fromJust mayben)

				n <- eguard (n < 0)
					(Error $ ":rm expected nonegative Int but got a negative one: " ++ show n)
					(n)

				return $ Remove n

			"limit" -> uneither $ do
				x <- eguard (null args)
					(Error $ "rm: expected single argument but got " ++ show (length args))
					(head args)

				let mayben = readMaybe x :: Maybe Int
				n <- eguard (isNothing mayben)
					(Error $ ":rm expected nonegative Int but got " ++ x)
					(fromJust mayben)

				n <- eguard (n < 0)
					(Error $ ":rm expected nonegative Int but got a negative one: " ++ show n)
					(n)

				return $ Limit n

			other ->
				ErrorNoCmd prefix

	where
	split = words line
	isEval = head line /= ':'
	prefix = tail (head split)
	args = tail split

