
module Compiler where

import Data.Either
import Data.List
import Control.Monad
import Data.IORef
import Data.Dynamic
import qualified Control.Concurrent.ParallelIO.Local as CONC
import qualified Control.Concurrent as CONC
import Control.Exception

import PatternT.Types
import PatternT.Util
import PatternT.Core
import PatternT.SimplifyInterface
import PatternT.Parsing
import PatternT.Display
import PatternT.MonadicRules

import ICompiler
import IUI
import Events

type SimplifyMonad = IO
type SimplifyCtx = ()
type MonadicSimplifyT = MonadicSimplify SimplifyMonad SimplifyCtx
type SimlifyFT = SimplificationF SimplifyMonad SimplifyCtx

simplifyCtxInitial :: SimplifyCtx
simplifyCtxInitial = ()

showCtx :: SimplifyCtx -> String
showCtx = show

builtinRules :: [PureSimplificationF]
builtinRules =
	[ ("$add", ruleAdd "$add")
	, ("$mult", ruleMult "$mult")
	]

splitLines :: String -> [String]
splitLines = lines

readPatterns :: String -> Either [ParseMatchError] [SimplifyPattern]
readPatterns text = do
	unless (null badRules) (Left badRules)
	return okRules
	where
	lines       = splitLines text
	mrules      = map parseMatch lines
	partitioned = partitionEithers mrules
	okRules     = snd partitioned
	badRules    = fst partitioned

mixedRules :: [SimplifyPattern] -> [SimlifyFT]
mixedRules patterns = map Tuple32 builtinRules ++ map Tuple30 patterns

data CompilerState = CompilerState
	{ compierStopped           :: Bool
	, compilerText             :: String
	, compilerPatterns         :: [SimplifyPattern]
	, evalCount                :: Int
	} deriving (Eq, Show, Read)

instance Reactor CompilerState CompilerEvent CompilerCtx where
	reactorStoppedQ = compierStopped
	reactorDelayMS = const 100
	-- reactorProcess = compilerProcess

data CompilerCtx = CompilerCtx
	{ execThreads   :: IORef [(Int, String, CONC.ThreadId)]
	, ebin          :: EventsBin
	}

-- compilerProcess :: CompilerState -> [ReaderEvent] -> IO (ReaderState, [Dynamic])

runSimplification :: String -> CompilerCtx -> CompilerState -> IO CompilerState
runSimplification line ctx state = do

	newth <- CONC.forkIO safeSimpthread
	atomicModifyIORef' (execThreads ctx) (\ threads -> ((currentEvalIndex, line, newth) : threads, ())) -- FIXME: datarace possible - if `newth` finishes too quickly, the removeFunc will do nothing and this thread will hang in list forever
	sendEvent (ebin ctx) (EvaluationStarted currentEvalIndex)

	return $ state { evalCount = currentEvalIndex }

	where
	simpthread = runSimplificationThread currentEvalIndex (ebin ctx) (mixedRules (compilerPatterns state)) line
	safeSimpthread = do
		x <- try simpthread
		handleResult x

	handleResult :: Either SomeException () -> IO ()
	handleResult x = do
		removeFunc
		case x of
			Right () ->
				return ()
			Left ex ->
				sendEvent (ebin ctx) (DebugLog $ "Simplification thread#" ++ show currentEvalIndex ++ " with expr = " ++ line ++ " failed with: " ++ show ex)

	currentEvalIndex = evalCount state + 1
	removeFunc = atomicModifyIORef' (execThreads ctx) modify
	modify threads = (filter ((/= currentEvalIndex) . fst3) threads, ())

threadsDoall :: CompilerCtx -> (Int -> IO ()) -> IO ()
threadsDoall ctx func = do
	threads <- readIORef (execThreads ctx)
	let indexes = map fst3 threads
	mapM_ func indexes

rerunSimplificationThread :: CompilerCtx -> CompilerState -> Int -> IO ()
rerunSimplificationThread ctx state index = do
	matched <- atomicModifyIORef' (execThreads ctx) modify
	mapM_ todo matched
	where
	todo (id, line, th) = do
		CONC.killThread th
		runSimplification line ctx state

	modify threads = partition ((== index) . fst3) threads

killRunningSimplification :: CompilerCtx -> Int -> IO ()
killRunningSimplification ctx index = do
	matched <- atomicModifyIORef' (execThreads ctx) modify
	mapM_ todo matched
	where
	todo (id, line, th) = do
		CONC.killThread th

	modify threads = partition ((== index) . fst3) threads

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

runSimplificationThread :: Int -> EventsBin -> [SimlifyFT] -> String -> IO ()
runSimplificationThread index ebin mixed line =
	case tokens of
		Left err ->
			sendEvent ebin (CompilerTokenizeError err)
		Right oktokens ->
			withTokens oktokens
	where
	tokens = tokenize line
	withTokens oktokens = do
		let tree = makeTree (Group oktokens)
		history <- mixedApplySimplificationsWithPureUntil0Debug mixed simplifyCtxInitial tree
		sendEvent ebin (PushEvaluation index line (showHistory history))

	showHistory :: [(Tree, Either SimplifyPattern String, SimplifyCtx)] -> [(String, String, String)]
	showHistory = map f
		where
		f (t, traceElem, ctx) = (stringifyTree t, stringifyTraceElem traceElem, showCtx ctx)
