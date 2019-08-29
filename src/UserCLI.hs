
module UserCLI where

import Text.Read (readMaybe)
import Data.Either
import Data.Maybe
import Data.List
import Data.Char
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Dynamic
import qualified Control.Concurrent as CONC

import Hacalc.PatternT
import ICompiler
import IUI
import IReader
import Events
import Util

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

	Remove is ->
		(state, concatMap (\ i -> [toDyn $ UIRemoveEvaluation i, toDyn $ CompilerRemoveEvalRecord i]) is)
	Reset ->
		(state, [toDyn $ ClearEvaluations, toDyn $ CompilerResetEvaluations])
	Limit n ->
		(state, [toDyn $ UIChangeLimit n, toDyn $ CompilerChangeLimit n])
	Whitespace ->
		(state, [])
	SetPadding n ->
		(state, [toDyn $ UISetPadding n])
	Help s ->
		(state, [toDyn $ Notify s])

data Cmd
	= Error String
	| ErrorNoCmd String
	| Eval String
	| Stop
	| Reset
	| Remove [Int]
	| Limit Int
	| Whitespace
	| SetPadding Int
	| Help String
	deriving (Eq, Show, Read)

eguard :: Bool -> a -> b -> Either a b
eguard bad a b = if bad then Left a else Right b

uneither :: Either x x -> x
uneither x = case x of
	Left a -> a
	Right b -> b

cmds :: [(String, [String] -> Cmd, String)]
cmds =
	[ ("quit"   , quit      , "Quit. Saves session if sessionfile provided")
	, ("reset"  , reset     , "Clears all evaluations. Kills the evaluation threads")
	, ("del"    , rmcmd     , "Removes evaluation, kills its evaluation thread")
	, ("limit"  , setLimit  , "Sets the limit of evaluations showed. Old ones will be killed")
	, ("padding", setPadding, "Set padding of reductions list")
	, ("help"   , help      , "Show list of commands")
	]

help :: [String] -> Cmd
help args = Help (concatMap format cmds)
	where
	maxnamelen = maximum $ map length $ map fst3 cmds
	format (name, f, description) = "\t:" ++ (padLeft ' ' maxnamelen name) ++ " \t" ++ description ++ "\n"

setPadding :: [String] -> Cmd
setPadding args = uneither $ do
	x <- eguard (null args)
		(Error $ "expected single argument but got " ++ show (length args))
		(head args)

	let mayben = readMaybe x :: Maybe Int
	n <- eguard (isNothing mayben)
		(Error $ "expected positive Int but got " ++ x)
		(fromJust mayben)

	n <- eguard (n <= 0)
		(Error $ "expected positive Int but got a negative one: " ++ show n)
		(n)

	return $ SetPadding n

setLimit :: [String] -> Cmd
setLimit args = uneither $ do
	x <- eguard (null args)
		(Error $ "expected single argument but got " ++ show (length args))
		(head args)

	let mayben = readMaybe x :: Maybe Int
	n <- eguard (isNothing mayben)
		(Error $ "expected nonegative Int but got " ++ x)
		(fromJust mayben)

	n <- eguard (n < 0)
		(Error $ "expected nonegative Int but got a negative one: " ++ show n)
		(n)

	return $ Limit n

rmcmd :: [String] -> Cmd
rmcmd args = uneither $ do
	let maybens = map (\ x -> readMaybe x :: Maybe Int) args
	ns <- eguard (any isNothing maybens)
		(Error $ "expected positive Ints but got " ++ show maybens)
		(map fromJust maybens)

	ns <- eguard (any (<= 0) ns)
		(Error $ "expected positive Ints but got a negative one: " ++ show ns)
		(ns)

	return $ Remove ns

reset :: [String] -> Cmd
reset args = Reset

quit :: [String] -> Cmd
quit args = Stop

cmdParse :: String -> Cmd
cmdParse line = do
	if all isSpace line
	then Whitespace
	else if isEval
		then Eval line
		else case find ((isPrefixOf prefix) . fst3) cmds of
			Just (name, f, desc) -> f args
			Nothing -> ErrorNoCmd prefix

	where
	split = words line
	isEval = head line /= ':' || null (tail line)
	prefix = tail (head split)
	args = tail split

